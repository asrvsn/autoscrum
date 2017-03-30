{-# LANGUAGE LambdaCase #-}

module AirtableIO.TasksBaseValidate 
  ( 
  -- * public API
    runValidator
  -- * Data cleaning
  , reconcileWithThreads
  , selectDescendantsOf
  , 
  ) where

import Control.Monad.State

import AirtableIO.TasksBase

type ValidateM = StateT (TasksBase, [ValidateError]) IO

data ValidateError = 
    ThrErr RecordID String

-- * public API

runValidator :: TasksBase -> IO TasksBase
runValidator base = 
  runValidateM base (foldl' (>>) (pure ()) validators)
  where
    validators = 
      [ positiveStoryPtFilter
      , threadStatusFilter
      , threadStatusGuard
      , taskImpossibilityGuard
      ]

-- * helpers

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

-- * validators 

positiveStoryPtFilter :: ValidateM ()
positiveStoryPtFilter = modifyBase $ \b -> 
  let thrTbl = threads b
      blkTbl = blocks b
      cntTbl = containments b
      thrTbl_ = deleteWhere thrTbl $ \rec -> 
                   not (threadAssignable (recordObj rec))
                || threadStoryPts (recordObj rec) <= 0
  in reconcileWithThreads thrTbl_ b

threadStatusFilter :: ValidateM ()
threadStatusFilter = modifyBase $ \b -> 
  let thrTbl_ = deleteWhere (threads base) $ \rec -> 
                  case threadStatus (recordObj rec) of 
                    Nothing -> False
                    Just s  -> 
                      case s of 
                        WorkingOn   -> True
                        Blocked     -> True 
                        OnPause     -> True
                        _           -> False
  in reconcileWithThreads thrTbl_ b

threadStatusGuard :: ValidateM ()
threadStatusGuard = do
  thrs <- gets (selectAll . threads)
  forM_ thrs $ \(thrId, thr) -> 
    let shouldBeAssigned = 
          case (threadAssignee thr) of 
            Nothing -> validateErr $ ThrErr thrId "Had a WorkingOn, Blocked, or OnPause status but had no assignee"
            Just _  -> okay
        okay = pure ()
    in  case (threadStatus thr) of 
          Nothing -> okay
          Just s  -> 
            case s of 
              WorkingOn   -> shouldBeAssigned 
              Blocked     -> shouldBeAssigned
              OnPause     -> shouldBeAssigned
              _           -> okay 

taskImpossibilityGuard :: ValidateM ()
taskImpossibilityGuard = do
  base <- gets fst
  case getImpossibleThreads base of
    [] -> pure ()
    xs -> forM_ xs $ \x -> 
      validateErr $ ThrErr x "Impossible to complete due to tag configuration"

-- * Data cleaning / validation

reconcileWithThreads :: Table Thread -> TasksBase -> TasksBase
reconcileWithThreads thrTbl_ base = 
  base { threads = thrTbl_, blocks = blkTbl_, containments = cntTbl_ }
  where
    blkTbl_ =  vDeleteWhere (blocks base) $ \blk -> 
                any (not . exists thrTbl_) [blockingThread blk, blockedThread blk] 
    cntTbl_ =  vDeleteWhere (containments base) $ \cnt -> 
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

selectDescendantsOf :: ThreadID -> TasksBase -> TasksBase
selectDescendantsOf thrId base = 
  case vSelectMaybe (threads base) thrId of 
    Nothing -> error "selectDescendantsOf: could not find thread "
    Just _  -> 
      let thrTbl_ = worker (fromList []) [thrId]
      in  reconcileWithThreads thrTbl_ base
  where
    insert (Table recs o) k v = Table (Map.insert k v recs) o
    worker thrTbl_ []     = thrTbl_ 
    worker thrTbl_ thrIds = 
      let childIds  = concatMap (getChildren (containments base)) thrIds
          childThrs = map (vSelect (threads base)) childIds 
          thrTbl_'  = foldr (uncurry (insert thrTbl_)) thrTbl_ (zip childIds childThrs)
      in worker thrTbl_' childIds
