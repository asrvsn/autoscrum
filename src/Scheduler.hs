module Scheduler 
  ( schedule ) where 

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

schedule :: BayesNet
        -> Table Thread
        -> Table Block
        -> Table Containment
        -> Table Developer 
        -> Table Tag 
        -> Table Velocity 
        -> ScheduleParams
        -> Schedule
schedule bn thrTbl blkTbl cntTbl devTbl tagTbl velTbl prms = 
  execState flushTasks initSchedule
  where
    devs  = selectAllKeys devTbl

    initSchedule = Map.fromList $ zip (map DevID devs) (repeat [])

    prioritization = computePrioritization bn thrTbl blkTbl cntTbl
    

    flushTasks :: State Schedule ()
    flushTasks = do
      ts <- getUnblockedTasks
      case ts of 
        [] -> return ()
        _  -> do
          let possibleAssignments = [(t, DevID d) | t <- ts, d <- devs]
          features <- mapM (uncurry assignmentFeatures) possibleAssignments
          let losses = map (loss prms) (rescaleFeatures features)
          let ((t, d), _) = minimumBy (compare `on` snd) (zip possibleAssignments losses)
          assign t d
          flushTasks

    getUnblockedTasks :: State Schedule [ThreadID]
    getUnblockedTasks = do
      completed <- gets completedTasks
      return $ map ThreadID $ 
        selectKeyWhere thrTbl $ \thrId thr -> 
          let blocks = Set.fromList (getBlockingThreads blkTbl thrTbl $ ThreadID thrId) 
          in    blocks `Set.isSubsetOf` completed
             && ThreadID thrId `Set.notMember` completed
             && not (threadFinished thr)

    assignmentFeatures :: ThreadID -> DevID -> State Schedule Features
    assignmentFeatures thrId devId = do
      -- (1) time to task unblocked (by dev availability as well as blocking threads)
      t_task <- getTaskAvailability thrId
      t_dev  <- getDevAvailability devId
      let t_unblocked = max t_task t_dev
      -- (2) dev completion time
      let t_elapsed = taskCompletionTime tagTbl thrTbl devTbl velTbl thrId devId   
      -- (3) task priority
      let priority = getPrioritization prioritization `lookup` thrId
      return $ Features {
          f_completed = t_unblocked + t_elapsed
        , f_priority = priority
        }

    assign :: ThreadID -> DevID -> State Schedule ()
    assign thrId devId = do
      let dt_task = taskCompletionTime tagTbl thrTbl devTbl velTbl thrId devId
      t_task <- getTaskAvailability thrId
      t_dev  <- getDevAvailability devId
      let elapsed = if t_task > t_dev 
                      then [(t_task + dt_task, Just thrId), (t_task, Nothing)]
                      else [(t_dev + dt_task, Just thrId)]
      modify $ 
        Map.adjust (\ts -> elapsed ++ ts) devId

    getTaskAvailability :: HasCallStack => ThreadID -> State Schedule Double
    getTaskAvailability thrId = do
      let blkThrs = getBlockingThreads blkTbl thrTbl thrId
      blkTimes <- mapM getCompletedTaskTime blkThrs
      return $ case blkTimes of 
        [] -> 0
        _  -> assertPositive $ maximum blkTimes

    getDevAvailability :: HasCallStack => DevID -> State Schedule Double
    getDevAvailability devId = gets $ \mp -> 
      case mp `lookup` devId of
        []   -> 0
        t:ts -> assertPositive $ fst t 

    getCompletedTaskTime :: ThreadID -> State Schedule Double
    getCompletedTaskTime thrId = gets $ \mp -> 
      let rec [] = error $ "could not find completed task " ++ show thrId ++ "in schedule"
          rec (timeline:rest) = case find (\(_, mode) -> Just thrId == mode) timeline of 
                                  Just (t_task, _) -> t_task
                                  Nothing -> rec rest
      in rec (Map.elems mp) 

    assertPositive :: HasCallStack => Double -> Double
    assertPositive i = if i > 0 then i else error "assertPositive failed"

-- helpers 

-- the lower the loss, the better the assigment
loss :: ScheduleParams -> Features -> Double
loss p f = 
  -- compute loss as a sum of weighted squares 
  sum [
      w_completed p * f_completed f'
    , w_priority p * f_priority f'
    ]
  where
    f' = (f + fromInteger 1) ** fromInteger 2 -- recenter and square

-- rescale features to [0,1] U {-Infinity, Infinity}
rescaleFeatures :: [Features] -> [Features]
rescaleFeatures fs = map (f_unop fromNaN . (\x -> (x - x_min) / (x_max - x_min))) fs
  where
    fromNaN a = if isNaN a then 0 else a
    x_min = f_nop (minimum . filter (not . isInfinite)) fs
    x_max = f_nop (maximum . filter (not . isInfinite)) fs
