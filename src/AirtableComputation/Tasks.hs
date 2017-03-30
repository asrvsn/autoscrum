{-# LANGUAGE NoImplicitPrelude #-}

module AirtableComputation.Tasks
  ( 
  -- * Types
    Prioritization
  -- * Bayes net computation
  , tasksBayesNet
  -- * Computation
  , taskCompletionTime
  , blockageFactor
  , taskPriority
  , prioritize
  ) where

import           Prelude hiding (lookup)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import           Data.Foldable (foldlM)
import           Data.Monoid
-- import           Data.Maybe (catMaybes)
import           Airtable.Table
import           Bayes (SBN)
import           Bayes.Factor 
import           Bayes.Factor.CPT (CPT)
import           Bayes.BayesianNetwork
import           Bayes.FactorElimination ( nodeComparisonForTriangulation
                                         , createJunctionTree
                                         )

import Missing
import AirtableIO.TasksBase
import AirtableComputation.BayesNet

-- * Types

type Prioritization = HashMap ThreadID Double

-- * Bayes net 

tasksBayesNet :: TasksBase -> BayesNet RecordID
tasksBayesNet base = 
  let varMapKeys = Set.fromList (Map.keys varMap)
  in if varMapKeys == varKeys
    then BayesNet varMap junctionTree
    else error $ 
         "invariant violation: the variable map has additional records \n" 
      <> show (varMapKeys `Set.difference` varKeys)
      <> "\n and the threads table has additional records \n"
      <> show (varKeys `Set.difference` varMapKeys)
  where
    thrTbl = threads base
    cntTbl = containments base

    junctionTree = createJunctionTree nodeComparisonForTriangulation net

    (varMap, net) = construction

    construction :: (HashMap RecordID (TDV Bool), SBN CPT)
    construction = runBN $ do
      vmap <- foldlM createVar Map.empty varKeys
      mapM_ (setContainment vmap) (vSelectAll cntTbl)
      -- initialize all root threads
      mapM_ (setCertainContainment vmap) rootKeys
      -- initialize all dis connected threads (this is for inconsistent data)
      mapM_ (setCertainContainment vmap) (varKeys `Set.difference` cntKeys)
      return vmap

    createVar mp thrId = do
      v <- variable (rec2str thrId) (undefined :: Bool)
      return $ Map.insert thrId v mp

    setContainment vmap cnt = do
      let parent = vmap `lookup` (parentThread cnt) 
      let child = vmap `lookup` (childThread cnt)
      let contP = containmentProbability cnt
      cpt child [parent] ~~ [
          1           -- p(child=F | parent=F)
        , 1 - contP   -- p(child=F | parent=T)
        , 0           -- p(child=T | parent=F)
        , contP       -- p(child=T | parent=T)
        ]

    setCertainContainment vmap thrId = 
      proba (vmap `lookup` thrId) ~~ [0, 1]

    varKeys = Set.fromList (selectAllKeys thrTbl)
    rootKeys = Set.filter (null . getParents cntTbl . ThreadID) varKeys
    cntKeys = 
        Set.fromList 
      . foldMap (\cnt -> [parentThread cnt, childThread cnt])
      $ vSelectAll cntTbl 

-- * Computation

taskCompletionTime :: TasksBase -> ThreadID -> DevID -> Double
taskCompletionTime base thrId devId = 
  storyPts / (product devMultipliers * product missingMultipliers)
  where
    dev = vSelect (developers base) devId
    thr = vSelect (threads base) thrId
    storyPts = threadStoryPts (vSelect (threads base) thrId)
    devMultipliers     = devMultipliersForTask (velocities base) devId thr 
    missingMultipliers = missingMultipliersForTask (velocities base) (tags base) dev thr

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
      let blkThr   = vSelect thrTbl blkThrId 
      in  if threadAssignable blkThr
            then blkPerc
            else case getChildren cntTbl blkThrId of
              [] -> blkPerc
              xs -> if all threadFinished (map (vSelect thrTbl) xs) 
                then 0
                else blkPerc


taskPriority :: TasksBase -> BayesNet RecordID
             -> ThreadID -- given thread ID
             -> Double
taskPriority base bn thrId = 
  case getBlockages blkTbl thrTbl thrId of 

    [] -> 
      let p = marginal bn (getThreadId thrId)
          thr = vSelect thrTbl thrId
          story = threadStoryPts thr
          blockage = blockageFactor thrTbl blkTbl cntTbl thrId 

      in (-1) * log (1 - (63/64) * p) / (story * blockage)

    xs -> 
      let blockagePriority blkThrId blkPerc = 
            taskPriority base bn blkThrId * blkPerc

      in sum $ map (uncurry blockagePriority) xs
  where
    blkTbl = blocks base
    thrTbl = threads base
    cntTbl = containments base

prioritize :: BayesNet RecordID -> TasksBase -> Prioritization
prioritize bn base = 
  Map.fromList $ zip tasks (map getPriority tasks)
  where
    tasks  = map ThreadID (selectAllKeys (threads base))
    getPriority = taskPriority base bn