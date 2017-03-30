{-# LANGUAGE NoImplicitPrelude #-}

module AirtableComputation.Tasks
  ( 
  -- * Types
    Prioritization
  -- * Data cleaning / validation
  , reconcileWithThreads
  , getImpossibleThreads
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
import           Data.Maybe (catMaybes)
import           Airtable.Table
import           Bayes (SBN)
import           Bayes.Factor 
import           Bayes.Factor.CPT (CPT)
import           Bayes.BayesianNetwork
import           Bayes.FactorElimination ( nodeComparisonForTriangulation
                                         , createJunctionTree
                                         , changeEvidence
                                         , posterior
                                         , JunctionTree
                                         )

import Missing
import AirtableIO.TasksBase
import AirtableComputation.BayesNet

-- * Types

type Prioritization = HashMap ThreadID Double

-- * Bayes net 

tasksBayesNet :: Table Thread 
             -> Table Containment 
             -> BayesNet RecordID
tasksBayesNet thrTbl cntTbl = 
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
      mapM_ (setContainment varMap) (vSelectAll cntTbl)
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
      $ vSelectAll cntTbl 

-- * Computation

taskCompletionTime :: Table Tag
                   -> Table Thread
                   -> Table Developer
                   -> Table Velocity
                   -> ThreadID 
                   -> DevID 
                   -> Double
taskCompletionTime tagTbl thrTbl devTbl velTbl thrId devId = 
  storyPts / (product devMultipliers * product missingMultipliers)
  where
    dev = vSelect devTbl devId
    thr = vSelect thrTbl thrId
    storyPts = threadStoryPts (vSelect thrTbl thrId)
    devMultipliers     = devMultipliersForTask velTbl devId thr 
    missingMultipliers = missingMultipliersForTask velTbl tagTbl dev thr

devMultipliersForTask :: Table Velocity -> DevID -> Thread -> [Double]
devMultipliersForTask velTbl devId thr = 
  map vMultiplier $ 
    vSelectWhere velTbl $ \vel -> 
          vTag vel `elem` threadTags thr 
      &&  DevID (vDeveloper vel) == devId

missingMultipliersForTask :: Table Velocity -> Table Tag -> Developer -> Thread -> [Double]
missingMultipliersForTask velTbl tagTbl dev thr = 
  map (tagMultiplierIfMissing . recordObj) $ 
    selectWhere tagTbl $ \rec ->
          recordId rec `elem` threadTags thr
      &&  TagID (recordId rec) `notElem` devTags velTbl dev

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


taskPriority :: Table Thread
             -> Table Block
             -> Table Containment
             -> BayesNet RecordID
             -> ThreadID -- given thread ID
             -> Double
taskPriority thrTbl blkTbl cntTbl bn thrId = 
  case getBlockages blkTbl thrTbl thrId of 

    [] -> 
      let p = marginal bn (getThreadId thrId)
          thr = vSelect thrTbl thrId
          story = threadStoryPts thr
          blockage = blockageFactor thrTbl blkTbl cntTbl thrId 

      in (-1) * log (1 - (63/64) * p) / (story * blockage)

    xs -> 
      let blockagePriority blkThrId blkPerc = 
            taskPriority thrTbl blkTbl cntTbl bn blkThrId * blkPerc

      in sum $ map (uncurry blockagePriority) xs

prioritize :: BayesNet RecordID
            -> Table Thread
            -> Table Block
            -> Table Containment
            -> Prioritization
prioritize bn thrTbl blkTbl cntTbl = 
  Map.fromList $ zip tasks (map getPriority tasks)
  where
    tasks  = map ThreadID (selectAllKeys thrTbl)
    getPriority = taskPriority thrTbl blkTbl cntTbl bn