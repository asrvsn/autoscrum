{-# LANGUAGE LambdaCase #-}

module AirtableIO.TasksBaseValidate 
  (
  ) where

import Control.Monad.State

import AirtableIO.TasksBase

type ValidateM = StateT (TasksBase, [ValidateError]) IO

data ValidateError = 
    ThrErr RecordID String

runValidateM :: TasksBase -> ValidateM () -> IO TasksBase
runValidateM base vm = do
  s <- execStateT vm (base, [])
  case s of 
    (base', []) -> return base'
    (_, errs) -> do
      mapM_ (putStrLn . showValidateError base) errs
      error "Validation errors, exiting." 

validateErr :: ValidateError -> ValidateM ()
validateErr e = modify $ \(b, errs) -> (b, e:errs)

modifyBase :: (TasksBase -> TasksBase) -> ValidateM ()
modifyBase f = modify $ \(b, errs) -> (f b, errs)

showValidateError :: TasksBase -> ValidateError -> String
showValidateError base = \case
  ThrErr rec err -> 
    "error in {" ++ (take 40 . threadName . vSelect (threads base)) rec ++ "}: " ++ err

runValidator :: TasksBase -> IO TasksBase
runValidator base = 
  runValidateM base (foldl' (>>) (pure ()) validators)
  where
    validators = 
      [ positiveStoryPtFilter
      , taskImpossibilityGuard
      ]

positiveStoryPtFilter :: ValidateM ()
positiveStoryPtFilter = modifyBase $ \b -> 
  let thrTbl = threads b
      blkTbl = blocks b
      cntTbl = containments b
      thrTbl_ = deleteWhere thrTbl_ $ \rec -> 
                   not (threadAssignable (recordObj rec))
                || threadStoryPts (recordObj rec) <= 0
      (blkTbl_, cntTbl_) = reconcileWithThreads thrTbl_ blkTbl cntTbl
  in b { threads = thrTbl_, blocks = blkTbl_, containments = cntTbl_ } 

taskImpossibilityGuard :: ValidateM ()
taskImpossibilityGuard = do
  base <- gets fst
  case getImpossibleThreads base of
    [] -> pure ()
    xs -> forM_ xs $ \x -> 
      validateErr $ ThrErr x "Impossible to complete due to tag configuration"

-- * Data cleaning / validation

reconcileWithThreads :: Table Thread -> Table Block -> Table Containment
                     -> (Table Block, Table Containment)
reconcileWithThreads thrTbl_ blkTbl_ cntTbl_ = 
  (blkTbl, cntTbl)
  where
    blkTbl =  vDeleteWhere blkTbl_ $ \blk -> 
                any (not . exists thrTbl_) [blockingThread blk, blockedThread blk] 
    cntTbl =  vDeleteWhere cntTbl_ $ \cnt -> 
                any (not . exists thrTbl_) [parentThread cnt, childThread cnt]

-- | Check that for every thread there exists a developer with non-infinite completion time.
getImpossibleThreads :: TasksBase -> [RecordID]
getImpossibleThreads base = 
  catMaybes $ map completionImpossibility (selectAllKeys thrTbl)
  where
    thrTbl = threads base
    velTbl = velocities base
    devTbl = developers base
    completionImpossibility thrId = 
      let thr = vSelect thrTbl thrId
          getMultipliers devId =     devMultipliersForTask velTbl devId thr 
                                  ++ missingMultipliersForTask velTbl tagTbl (vSelect devTbl devId) thr
          multipliers = [getMultipliers (DevID devId) | devId <- selectAllKeys devTbl]
      in  if any (any (> 0)) multipliers
            then Nothing
            else Just thrId