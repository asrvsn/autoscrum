{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import           Types
import           Constants

import           System.Random
import           Network.Wreq
import           Data.Monoid
import           Data.List (find, minimumBy)
import           Data.Aeson (FromJSON)
import           Data.Maybe (catMaybes)
import           Data.Foldable (foldlM)
import           Data.Function (on)
import           Data.Traversable (for)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import           Control.Monad.State
import           Bayes.BayesianNetwork


-- API methods

getTable :: (FromJSON a) => String -> IO (Table a)
getTable tblStr = getTableParts opts url
  where
    opts = defaults & header "Authorization" .~ [api_key] 
                    & param "view" .~ ["Main View"]
    url  = api_url <> tblStr

getTableParts :: Options -> String -> IO (Table a)
getTableParts opts url = do
  resp <- getWith opts url
  getMore (fromResp resp)
  where
    getMore tbl = case tableOffset tbl of 
      Just offset -> do
        resp <- getWith (opts & param "offset" .~ [offset]) url
        getMore $ tbl <> fromResp resp
      Nothing -> 
        pure tbl

fromResp :: Response ByteString -> Table a
fromResp r = decode $ r ^. responseBody

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
  map childThread $ 
    selectWhere cntTbl $ \_ cnt -> 
      parentThread cnt == thrId

getParents :: Table Containment 
           -> ThreadID  
           -> [ThreadID]
getParents cntTbl thrId = 
  map parentThread $ 
    selectWhere cntTbl $ \_ cnt -> 
      childThread cnt == thrId

getBlockages :: Table Block 
             -> ThreadID 
             -> [(ThreadID, Double)]
getBlockages blkTbl thrId = 
  map (\b -> (blockedThread b, blockPercentage b)) blk $ 
    selectWhere blkTbl $ \_ blk -> 
      blockingThread blk == thrId

getBlockedThreads :: Table Block 
                  -> ThreadID
                  -> [ThreadID]
getBlockedThreads blkTbl thrId = 
  map blockedThread blk $ 
    selectWhere blkTbl $ \_ blk -> 
      blockingThread blk == thrId

getBlockingThreads :: Table Block 
                    -> ThreadID
                    -> [ThreadID]
getBlockingThreads blkTbl thrId = 
  map blockingThread blk $ 
    selectWhere blkTbl $ \_ blk -> 
      blockedThread blk == thrId

getMasterThread :: Table Thread -> ThreadID
getMasterThread thrTbl = thrId
  where
    [thrId] = selectKeyWhere thrTbl $ \_ thr -> 
                threadName thr == master_thread_name

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
blockageFactor thrTbl blkTbl cntTbl (ThreadID thrId) = 
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
           -> ThreadID 
           -> IO [ThreadID]
taskSample thrTbl cntTbl thrId = 
  sample $ 
    map 
      (\thr -> 
        (thr, inferMarginalInclusion thrTbl cntTbl thr))
      (getDescendants thrId)

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

inferMarginalInclusion :: Table Thread
                       -> Table Containment
                       -> ThreadID -- Parent task
                       -> ThreadID -- Descendant task
                       -> Double   -- P(descendant task | parent task=T)
inferMarginalInclusion thrTbl cntTbl parentId childId = 
  case posterior junctionTree [varMap Map.! childId] of 
    Just p  -> p
    Nothing -> error "could not find cluster for given thread ID"

  where
    junctionTree = 
      updateEvidence [(varMap Map.! parentId) =: True] $ 
        createJunctionTree nodeComparisonForTriangulation net

    (varMap, net) = runBN $ do
      varMap <- createVars 
      mapM_ (addFactor varMap . snd) (toList cntTbl)
      return varMap

    createVars = do
      let thrIds = map fst (toList thrTbl)
      let mkVar mp tid = do var <- variable tid (undefined :: Bool)
                            return $ Map.insert tid var mp
      foldlM mkVar Map.empty thrIds

    addFactor varMap cnt = do
      let parent = varMap Map.! (parentThread cnt) 
      let child = varMap Map.! (childThread cnt)
      let contP = containmentProbability cnt
      cpt child [parent] ~~ [
          1           -- p(child=F | parent=F)
        , 1 - contP   -- p(child=F | parent=T)
        , 0           -- p(child=T | parent=F)
        , contP       -- p(child=T | parent=T)
        ]

taskPriority :: Table Thread
             -> Table Block
             -> Table Containment
             -> ThreadID -- master thread ID
             -> ThreadID -- given thread ID
             -> Double
taskPriority thrTbl blkTbl cntTbl masterThrId thrId = 
  case getBlockages blkTbl thrId of 

    [] -> 
      let p = inferMarginalInclusion thrTbl cntTbl masterThrId thrId
          thr = select thrTbl (getThreadId thrId)
          story = threadStoryPts thr
          blockage = blockageFactor thrTbl blkTbl cntTbl thrId 

      in (-1) * log (1 - (15/16) * p) / (story * blockage)

    xs -> sum $ for xs $ \(blkThrId, blkPerc) -> 
      taskPriority thrTbl blkTbl cntTbl masterThrId blkThrId * blkPerc

computePriorities :: Table Thread
                 -> Table Block
                 -> Table Containment
                 -> IO [Priority]
computePriorities thrTbl blkTbl cntTbl = 
  forM tasks $ \task -> do
    let priority = taskPriority thrTbl blkTbl cntTbl masterThrId task
    putStrLn $ "Computed priority " ++ show priority ++ " for " ++ show task
    return $ Priority task priority
  where
    tasks       = map ThreadID $ selectKeyWhere thrTbl $ \_ thr -> threadAssignable thr
    masterThrId = getMasterThread thrTbl

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
      let priority = taskPriority thrTbl blkTbl cntTbl masterThrId thrId
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
    getDevAvailability devId = gets (\mp -> fst $ last (mp Map.! devId))

    getCompletedTaskTime :: ThreadID -> StateT Schedule IO Double
    getCompletedTaskTime thrId = gets $ \mp -> 
      let rec [] = error $ "could not find completed task " ++ show thrId ++ "in schedule"
          rec (timeline:rest) = case find (\(_, mode) -> Just thrId == mode) timeline of 
                                  Just (t_task, _) -> t_task
                                  Nothing -> rec rest
      in rec (Map.elems mp) 