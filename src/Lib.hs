{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import           Types
import           Missing
import           Constants

import           Prelude hiding (lookup)

import           GHC.Stack
import           System.Random
import           System.Directory (doesFileExist)
import           System.Process (system)
import           System.Exit (ExitCode(..))
import           Network.Wreq
import           Airtable.Table
import           Airtable.Query
import           Control.Lens ((^.), (.~), (&))
import           Text.Read (readMaybe)
import           Data.Monoid
import           Data.Hashable
import           Data.List (find, minimumBy)
import           Data.Aeson (FromJSON, ToJSON, eitherDecode)
import           Data.Maybe (catMaybes)
import           Data.Foldable (foldlM)
import           Data.Function (on)
import           Data.Traversable (for)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import           Control.Monad.State

-- Helpers

devTags :: Table Velocity
        -> Developer
        -> [TagID] 
devTags velTbl = 
  map (TagID . vTag . select velTbl) . devVelocities 

getChildren :: Table Containment
            -> ThreadID 
            -> [ThreadID]
getChildren cntTbl thrId = 
  map (ThreadID . childThread) $ 
    selectWhere cntTbl $ \_ cnt -> 
      parentThread cnt == getThreadId thrId

getParents :: Table Containment 
           -> ThreadID  
           -> [ThreadID]
getParents cntTbl thrId = 
  map (ThreadID . parentThread) $ 
    selectWhere cntTbl $ \_ cnt -> 
      childThread cnt == getThreadId thrId

getDescendants :: Table Containment
               -> ThreadID 
               -> [ThreadID]
getDescendants cntTbl thrId = 
  Set.toList $ rec Set.empty (getChildren cntTbl thrId) 
  where
    rec a [] = a
    rec a cs = rec (foldr Set.insert a cs) (foldMap (getChildren cntTbl) cs)

getBlockages :: Table Block 
              -> Table Thread
             -> ThreadID 
             -> [(ThreadID, Double)]
getBlockages blkTbl thrTbl thrId = 
  map (\b -> (ThreadID $ blockedThread b, blockPercentage b)) $ 
    selectWhere blkTbl $ \_ blk -> 
         blockingThread blk == getThreadId thrId
      && not (threadFinished $ select thrTbl (blockedThread blk))

getBlockedThreads :: Table Block 
                  -> Table Thread
                  -> ThreadID
                  -> [ThreadID]
getBlockedThreads blkTbl thrTbl thrId = 
  map (ThreadID . blockedThread) $ 
    selectWhere blkTbl $ \_ blk -> 
         blockingThread blk == getThreadId thrId
      && not (threadFinished $ select thrTbl (blockedThread blk))

getBlockingThreads :: Table Block 
                    -> Table Thread
                    -> ThreadID
                    -> [ThreadID]
getBlockingThreads blkTbl thrTbl thrId = 
  map (ThreadID . blockingThread) $ 
    selectWhere blkTbl $ \_ blk -> 
         blockedThread blk == getThreadId thrId
      && not (threadFinished $ select thrTbl thrId)

getMasterThread :: Table Thread -> ThreadID
getMasterThread thrTbl = ThreadID thrId
  where
    [thrId] = selectKeyWhere thrTbl $ \_ thr -> 
                threadName thr == master_thread_name

-- Data cleaning

reconcileWithThreads :: Table Thread -> Table Block -> Table Containment
                     -> (Table Block, Table Containment)
reconcileWithThreads thrTbl_ blkTbl_ cntTbl_ = 
  (blkTbl, cntTbl)
  where
    blkTbl =  deleteWhere blkTbl_ $ \_ blk -> 
                any (not . exists thrTbl_) [blockingThread blk, blockedThread blk] 
    cntTbl =  deleteWhere cntTbl_ $ \_ cnt -> 
                any (not . exists thrTbl_) [parentThread cnt, childThread cnt]

-- task sampling

taskSample :: BayesNet -> Table Thread -> IO Table Thread
taskSample bn thrTbl_ = do
  let thrIds_ = selectAllKeys thrTbl_ 
  let thrPs   = map (marginal bn) thrIds
  thrIds <- sample (zip thrPs thrIds_)
  return $ 
    deleteWhere thrTbl_ $ \rec _ -> 
      rec `notElem` thrIds

sample :: [(Double, a)] -> IO [a]
sample pdf = catMaybes <$> mapM event pdf
  where
    event (p, a) = do
      outcome <- unfairCoin p
      return $ 
        if outcome then Just a else Nothing 

-- bayes net stuff

bayesNet :: Table Thread 
         -> Table Containment 
         -> BayesNet RecordID
bayesNet thrTbl cntTbl = 
  let varMapKeys = Set.fromList (Map.keys varMap)
  in if varMapKeys == varKeys
    then BayesNet varMap junctionTree
    else error $ 
         "invariant violation: the variable map has additional records \n" 
      <> show (varMapKeys `Set.difference` varKeys)
      <> "\n and the threads table has additional records \n"
      <> show (varKeys `Set.difference` varMapKeys)
  where
    junctionTree = createJunctionTree nodeComparisonForTriangulation net

    (varMap, net) = construction

    construction :: (HashMap RecordID (TDV Bool), SBN CPT)
    construction = runBN $ do
      varMap <- foldlM createVar Map.empty varKeys
      mapM_ (setContainment varMap) (selectAll cntTbl)
      -- initialize all root threads
      mapM_ (setCertainContainment varMap) rootKeys
      -- initialize all dis connected threads (this is for inconsistent data)
      mapM_ (setCertainContainment varMap) (varKeys `Set.difference` cntKeys)
      return varMap

    createVar mp thrId = do
      var <- variable (rec2str thrId) (undefined :: Bool)
      return $ Map.insert thrId var mp

    setContainment varMap cnt = do
      let parent = varMap `lookup` (parentThread cnt) 
      let child = varMap `lookup` (childThread cnt)
      let contP = containmentProbability cnt
      cpt child [parent] ~~ [
          1           -- p(child=F | parent=F)
        , 1 - contP   -- p(child=F | parent=T)
        , 0           -- p(child=T | parent=F)
        , contP       -- p(child=T | parent=T)
        ]

    setCertainContainment varMap thrId = 
      proba (varMap `lookup` thrId) ~~ [0, 1]

    varKeys = Set.fromList (selectAllKeys thrTbl)
    rootKeys = Set.filter (null . getParents cntTbl . ThreadID) varKeys
    cntKeys = 
        Set.fromList 
      . foldMap (\cnt -> [parentThread cnt, childThread cnt])
      $ selectAll cntTbl 

marginal :: (Show a, HasCallStack) => BayesNet a -> a -> Double
marginal bn a = 
  factorNorm $ 
    case posterior (juncTree bn) [netVars bn `lookup` a] of 
      Just p  -> p
      Nothing -> error $ "could not find cluster for given thread ID: " <> show a

-- Computation

taskCompletionTime :: Table Tag
                   -> Table Thread
                   -> Table Developer
                   -> Table Velocity
                   -> ThreadID 
                   -> DevID 
                   -> Double
taskCompletionTime tagTbl thrTbl devTbl velTbl (ThreadID thrId) (DevID devId) = 
  storyPts / (product devMultipliers * product missingMultipliers)
  where
    dev = select devTbl devId
    thr = select thrTbl thrId
    storyPts = threadStoryPts (select thrTbl thrId)
    devMultipliers = map vMultiplier $ 
      selectWhere velTbl $ \_ v -> 
            vTag v `elem` threadTags thr 
        &&  vDeveloper v == devId
    missingMultipliers = map tagMultiplierIfMissing $ 
      selectWhere tagTbl $ \rec _ ->
            rec `elem` threadTags thr
        &&  TagID rec `notElem` devTags velTbl dev

blockageFactor :: Table Thread
               -> Table Block
               -> Table Containment
               -> ThreadID
               -> Double
blockageFactor thrTbl blkTbl cntTbl thrId = 
  product $ map (\b -> 1 / (1 - b)) blockages
  where
    blockages = map compute (getBlockages blkTbl thrTbl thrId)
    compute (blkThrId, blkPerc) = 
      let blkThr   = select thrTbl blkThrId 
      in  if threadAssignable blkThr
            then blkPerc
            else case getChildren cntTbl blkThrId of
              [] -> blkPerc
              xs -> if all threadFinished (map (select thrTbl) xs) 
                then 0
                else blkPerc


taskPriority :: Table Thread
             -> Table Block
             -> Table Containment
             -> BayesNet RecordID
             -> ThreadID -- given thread ID
             -> Double
taskPriority thrTbl blkTbl cntTbl bn thrId = 
  case getBlockages blkTbl thrTbl thrId of 

    [] -> 
      let p = marginalInclusion bn thrId
          thr = select thrTbl thrId
          story = threadStoryPts thr
          blockage = blockageFactor thrTbl blkTbl cntTbl thrId 

      in (-1) * log (1 - (63/64) * p) / (story * blockage)

    xs -> 
      let blockagePriority blkThrId blkPerc = 
            taskPriority thrTbl blkTbl cntTbl bn blkThrId * blkPerc

      in sum $ map (uncurry blockagePriority) xs

prioritize :: BayesNet
            -> Table Thread
            -> Table Block
            -> Table Containment
            -> Prioritization
prioritize bn thrTbl blkTbl cntTbl = 
  Prioritization . Map.fromList $ 
    zip tasks (map getPriority tasks)
  where
    tasks  = map ThreadID (selectAllKeys thrTbl)
    getPriority = taskPriority thrTbl blkTbl cntTbl bn

