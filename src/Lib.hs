{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import           Types
import           Constants

import           Prelude hiding (lookup)

import           System.Random
import           Network.Wreq
import           Control.Lens ((^.), (.~), (&))
import           Data.Monoid
import           Data.Hashable
import           Data.List (find, minimumBy)
import           Data.Aeson (FromJSON, eitherDecode)
import           Data.Maybe (catMaybes)
import           Data.Foldable (foldlM)
import           Data.Function (on)
import           Data.Traversable (for)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import           Control.Monad.State
import           Bayes (SBN)
import           Bayes.Factor 
import           Bayes.Factor.CPT (CPT)
import           Bayes.BayesianNetwork
import           Bayes.FactorElimination ( nodeComparisonForTriangulation
                                         , createJunctionTree
                                         , changeEvidence
                                         , posterior
                                         )


-- API methods

getTable :: (FromJSON a) => String -> IO (Table a)
getTable tblStr = getTableParts opts url <* putStrLn ("Downloaded " <> tblStr)
  where
    opts = defaults & header "Authorization" .~ ["Bearer " <> api_key] 
                    & param "view" .~ ["Main View"]
    url  = api_url <> tblStr

getTableParts :: (FromJSON a) => Options -> String -> IO (Table a)
getTableParts opts url = do
  resp <- getWith opts url
  getMore (fromResp resp)
  where
    getMore tbl = case tableOffset tbl of 
      Just offset -> do
        resp <- getWith (opts & param "offset" .~ [offset]) url
        putStrLn $ "Part " <> show offset
        getMore $ fromResp resp <> tbl
      Nothing -> 
        pure tbl

fromResp :: (FromJSON a) => Response ByteString -> Table a
fromResp r = decoder $ r ^. responseBody
  where
    decoder b = case eitherDecode b of 
      Left e -> error $ e <> "\nSource string: " <> show b
      Right r -> r

uploadPriority :: Priority -> IO ()
uploadPriority = undefined

uploadSchedule :: Schedule -> IO ()
uploadSchedule = undefined

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
             -> ThreadID 
             -> [(ThreadID, Double)]
getBlockages blkTbl thrId = 
  map (\b -> (ThreadID $ blockedThread b, blockPercentage b)) $ 
    selectWhere blkTbl $ \_ blk -> 
      blockingThread blk == getThreadId thrId

getBlockedThreads :: Table Block 
                  -> ThreadID
                  -> [ThreadID]
getBlockedThreads blkTbl thrId = 
  map (ThreadID . blockedThread) $ 
    selectWhere blkTbl $ \_ blk -> 
      blockingThread blk == getThreadId thrId

getBlockingThreads :: Table Block 
                    -> ThreadID
                    -> [ThreadID]
getBlockingThreads blkTbl thrId = 
  map (ThreadID . blockingThread) $ 
    selectWhere blkTbl $ \_ blk -> 
      blockedThread blk == getThreadId thrId

getMasterThread :: Table Thread -> ThreadID
getMasterThread thrTbl = ThreadID thrId
  where
    [thrId] = selectKeyWhere thrTbl $ \_ thr -> 
                threadName thr == master_thread_name

-- Data cleaning

reconcileThreads :: Table Containment 
                  -> Table Block 
                  -> Table Thread 
                  -> Table Thread
reconcileThreads cntTbl blkTbl thrTbl =
  deleteWhere thrTbl $ \_ thr -> 
       any (not . exists cntTbl) (threadContainments thr)
    || any (not . exists blkTbl) (threadBlocks thr)

-- Computation

taskCompletionTime :: Table Tag
                   -> Table Thread
                   -> Table Developer
                   -> Table Velocity
                   -> ThreadID 
                   -> DevID 
                   -> Double
taskCompletionTime tagTbl thrTbl devTbl velTbl (ThreadID thrId) (DevID devId) = 
  storyPts / product devMultipliers * product missingMultipliers
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
    blockages = map compute (getBlockages blkTbl thrId)
    compute (blkThrId, blkPerc) = 
      let blkThr   = select thrTbl blkThrId 
      in  if threadAssignable blkThr
            then blkPerc
            else case getChildren cntTbl blkThrId of
              [] -> blkPerc
              xs -> if all threadFinished (map (select thrTbl) xs) 
                then 0
                else blkPerc

taskSample :: Table Thread
           -> Table Containment
           -> ThreadID -- parent thread
           -> IO [ThreadID]
taskSample thrTbl cntTbl parentThrId = 
  sample $ 
    map 
      (\childThrId -> 
        (marginalInclusion thrTbl cntTbl childThrId, childThrId))
      (getDescendants cntTbl parentThrId)

sample :: [(Double, a)] -> IO [a]
sample pdf = catMaybes <$> mapM event pdf
  where
    event (p, a) = do
      outcome <- unfairCoin p
      return $ 
        if outcome then Just a else Nothing 

unfairCoin :: Double -> IO Bool
unfairCoin p = flip <$> getStdRandom (randomR (1,100))
  where
    flip r = r < (p * 100) 

marginalInclusion :: Table Thread
                 -> Table Containment
                 -> ThreadID -- Descendant task
                 -> Double   -- P(descendant task | parent task=T)
marginalInclusion thrTbl cntTbl childId = 
  factorNorm marginalCPT

  where
    marginalCPT = case posterior junctionTree [varMap `lookup` (getThreadId childId)] of 
      Just p  -> p
      Nothing -> error $ "could not find cluster for given thread ID: " <> show childId

    junctionTree = createJunctionTree nodeComparisonForTriangulation net

    (varMap, net) = construction

    construction :: (HashMap RecordID (TDV Bool), SBN CPT)
    construction = runBN $ do
      varMap <- foldlM createVar Map.empty varKeys
      mapM_ (setContainment varMap) (selectAll cntTbl)
      -- initialize all root threads
      let rootKeys = filter (null . getParents cntTbl . ThreadID) varKeys
      mapM_ (setRootContainment varMap) rootKeys
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

    setRootContainment varMap thrId = 
      proba (varMap `lookup` thrId) ~~ [0, 1]

    varKeys = 
        Set.toList 
      . Set.fromList 
      . foldMap (\cnt -> [parentThread cnt, childThread cnt])
      $ selectAll cntTbl 

    masterThrId = getMasterThread thrTbl

taskPriority :: Table Thread
             -> Table Block
             -> Table Containment
             -> ThreadID -- given thread ID
             -> Double
taskPriority thrTbl blkTbl cntTbl thrId = 
  case getBlockages blkTbl thrId of 

    [] -> 
      let p = marginalInclusion thrTbl cntTbl thrId
          thr = select thrTbl thrId
          story = threadStoryPts thr
          blockage = blockageFactor thrTbl blkTbl cntTbl thrId 

      in (-1) * log (1 - (15/16) * p) / (story * blockage)

    xs -> 
      let blockagePriority blkThrId blkPerc = 
            taskPriority thrTbl blkTbl cntTbl blkThrId * blkPerc

      in sum $ map (uncurry blockagePriority) xs

computePriorities :: Table Thread
                 -> Table Block
                 -> Table Containment
                 -> IO [Priority]
computePriorities thrTbl blkTbl cntTbl = 
  forM tasks $ \task -> do
    let priority = taskPriority thrTbl blkTbl cntTbl task
    putStrLn $ "Computed priority " ++ show priority ++ " for " ++ debug (select thrTbl task)
    return $ Priority task priority
  where
    tasks       = map ThreadID $ selectKeyWhere thrTbl $ \_ thr -> threadAssignable thr

computeSchedule :: Table Thread
         -> Table Block
         -> Table Containment
         -> Table Developer 
         -> Table Tag 
         -> Table Velocity 
         -> ScheduleParams
         -> IO Schedule
computeSchedule thrTbl blkTbl cntTbl devTbl tagTbl velTbl prms = 
  execStateT lptSchedule Map.empty
  where
    devs  = selectAllKeys devTbl
    tasks = selectKeyWhere thrTbl $ \_ thr -> threadAssignable thr

    masterThrId = getMasterThread thrTbl

    lptSchedule :: StateT Schedule IO ()
    lptSchedule = do
      ts <- getUnblockedTasks
      case ts of 
        [] -> return ()
        _  -> do
          let possibleAssignments = [(t, DevID d) | t <- ts, d <- devs]
          rs <- mapM (uncurry assignmentRank) possibleAssignments
          let ((t, d), _) = minimumBy (compare `on` snd) (zip possibleAssignments rs)
          assign t d
          lptSchedule

    getUnblockedTasks :: StateT Schedule IO [ThreadID]
    getUnblockedTasks = do
      completed <- gets $ Set.fromList . catMaybes . map snd . concat . Map.elems
      return $ map ThreadID $ 
        selectKeyWhere thrTbl $ \thr _ -> 
          let blocks = Set.fromList (getBlockingThreads blkTbl $ ThreadID thr) 
          in blocks `Set.isSubsetOf` completed

    -- the lower the rank, the better the assigment
    assignmentRank :: ThreadID -> DevID -> StateT Schedule IO Double
    assignmentRank thrId devId = do
      -- (1) time to task unblocked (by dev availability as well as blocking threads)
      t_task <- getTaskAvailability thrId
      t_dev  <- getDevAvailability devId
      let t_unblocked = max t_task t_dev
      -- (2) dev completion time
      let t_elapsed = taskCompletionTime tagTbl thrTbl devTbl velTbl thrId devId   
      -- (3) task priority
      let priority = taskPriority thrTbl blkTbl cntTbl thrId
      return $ 
          w_unblocked prms * t_unblocked
        + w_elapsed prms * t_elapsed
        + w_priority prms * priority

    assign :: ThreadID -> DevID -> StateT Schedule IO ()
    assign thrId devId = do
      let dt_task = taskCompletionTime tagTbl thrTbl devTbl velTbl thrId devId
      t_task <- getTaskAvailability thrId
      t_dev  <- getDevAvailability devId
      let elapsed = if t_task > t_dev 
                      then [(t_task, Nothing), (t_task + dt_task, Just thrId)]
                      else [(t_dev + dt_task, Just thrId)]
      modify $ 
        Map.adjust (\ts -> ts ++ elapsed) devId
      liftIO $ putStrLn $ "Assigned " ++ show thrId ++ " to " ++ show devId

    getTaskAvailability :: ThreadID -> StateT Schedule IO Double
    getTaskAvailability thrId = do
      let blkThrs = getBlockingThreads blkTbl thrId
      blkTimes <- mapM getCompletedTaskTime blkThrs
      return $ maximum blkTimes

    getDevAvailability :: DevID -> StateT Schedule IO Double
    getDevAvailability devId = gets (\mp -> fst $ last (mp `lookup` devId))

    getCompletedTaskTime :: ThreadID -> StateT Schedule IO Double
    getCompletedTaskTime thrId = gets $ \mp -> 
      let rec [] = error $ "could not find completed task " ++ show thrId ++ "in schedule"
          rec (timeline:rest) = case find (\(_, mode) -> Just thrId == mode) timeline of 
                                  Just (t_task, _) -> t_task
                                  Nothing -> rec rest
      in rec (Map.elems mp) 