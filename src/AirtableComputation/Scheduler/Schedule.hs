{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module AirtableComputation.Scheduler.Schedule 
  ( 
  -- * Scheduler types
    Schedule
  , ScheduleParams(..)
  -- * Scheduler API
  , sampledScheduleSummary
  , schedule 
  -- * Schedule accessors
  , getRuntime
  , getWorkingTime
  , getBlockedTime
  , convertToDiffedRuntime
  , completedTasks
  -- * Schedule visualization
  , ScheduleVis(..)
  , schedule2vis
  ) where 

import           Prelude hiding (lookup)
import           GHC.Generics
import           GHC.Stack
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Aeson
import           Data.Maybe (isJust, catMaybes)
import           Data.Foldable (find)
import           Data.Function (on)
import           Data.List (minimumBy)
import           Control.Monad.State
import           Airtable.Table

import Types
import Missing
import AirtableIO.TasksBase
import AirtableComputation.Scheduler.Features
import AirtableComputation.BayesNet
import AirtableComputation.Tasks

-- * Scheduler types

type Schedule = Map.HashMap DevID [(Double, Maybe ThreadID)]

instance Debug (Table Developer, Schedule) where
  debug (devTbl, s) = 
       prettyRows 20 (map getRow $ Map.toList s)
    ++ "\nTOTAL RUNTIME: " ++ show (getRuntime s)
    where
      getRow (devId, timeline) = [
          debug (vSelect devTbl (getDevId devId))
        , "working: " ++ show (getWorkingTime s devId)
        , "blocked: " ++ show (getBlockedTime s devId) 
        ]

data ScheduleParams = ScheduleParams { 
    w_completed :: Double
  , w_priority :: Double
  } deriving (Read, Show)

instance Debug ScheduleParams where
  debug prms = prettyRows 20 [
      ["w_completed", show $ w_completed prms]
    , ["w_priority", show $ w_priority prms]
    ]

data ScheduleSummary = ScheduleSummary
  { sched20 :: Schedule 
  , sched50 :: Schedule
  , sched80 :: Schedule
  }

-- * Scheduler API

sampledScheduleSummary :: Int
                       -> ScheduleParams
                       -> Table Thread
                       -> Table Block
                       -> Table Containment
                       -> Table Developer
                       -> Table Tag
                       -> Table Velocity
                       -> IO ScheduleSummary
sampledScheduleSummary nSamples prms thrTbl blkTbl cntTbl devTbl tagTbl velTbl = do
  let bn = bayesNet thrTbl cntTbl 
  schedules <- forM [1..nSamples] $ \i -> do
    putStrLn $ "Sample " <> show i <> " ..."
    thrTbl_ <- sampleTable bn thrTbl
    let (blkTbl_, cntTbl_) = reconcileWithThreads thrTbl_ blkTbl cntTbl 
    return $ schedule bn thrTbl_ blkTb_ cntTbl_ devTbl tagTbl velTbl prms

  let timedSchedules =  sortBy (compare `on` snd) $
                          zip schedules (map getRuntime schedules)
  return ScheduleSummary 
    { sched20 = timedSchedules !! (round $ n_schedule_samples * 0.2)
    , sched50 = timedSchedules !! (round $ n_schedule_samples * 0.5)
    , sched80 = timedSchedules !! (round $ n_schedule_samples * 0.8)
    }

schedule :: BayesNet RecordID
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

    prioritization = prioritize bn thrTbl blkTbl cntTbl
    

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
        selectKeyWhere thrTbl $ \rec -> 
          let thr     = recordObj rec
              thrId   = recordId rec
              blocks  = Set.fromList (getBlockingThreads blkTbl thrTbl $ ThreadID thrId) 
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
      let priority = prioritization `lookup` thrId
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

-- * Schedule accessors

getRuntime :: Schedule -> Double
getRuntime = maximum . map (head . map fst) . Map.elems

getWorkingTime :: Schedule -> DevID -> Double
getWorkingTime mp devId = 
  sum . map fst . filter (\(_,m) -> isJust m) $ (convertToDiffedRuntime mp) `lookup` devId

getBlockedTime :: Schedule -> DevID -> Double
getBlockedTime mp devId = 
  sum . map fst . filter (\(_,m) -> m == Nothing) $ (convertToDiffedRuntime mp) `lookup` devId

convertToDiffedRuntime :: Schedule -> Schedule 
convertToDiffedRuntime = Map.map (rec [])
  where
    rec a [] = a
    rec a ((t1, mode1):[]) = (t1, mode1):a
    rec a ((t2, mode2):(t1, mode1):rest) = 
      rec ((t2 - t1, mode2):a) ((t1, mode1):rest)

completedTasks :: Schedule -> Set ThreadID
completedTasks = Set.fromList . catMaybes . map snd . concat . Map.elems

-- * Schedule visualization

data ScheduleVis = ScheduleVis 
  { getVis :: Map.HashMap DevName [(Double, Maybe ThreadName)] 
  } deriving (Generic)

schedule2vis :: Table Thread 
              -> Table Developer 
              -> Schedule 
              -> ScheduleVis
schedule2vis thrTbl devTbl = 
  ScheduleVis . Map.fromList . map replaceField . Map.toList . convertToDiffedRuntime
  where
    replaceField (devId, timeline) = 
      (devName $ vSelect devTbl devId, map replaceThrName timeline)
    replaceThrName (t, mThrId) = 
      (t, fmap (threadName . vSelect thrTbl) mThrId)

instance Show ScheduleVis where
  show = BLC.unpack . encode 

instance ToJSON ScheduleVis where
  toJSON = object . map fromField . Map.toList . getVis
    where
      fromField ((DevName dev), timeline) = dev .= timeline