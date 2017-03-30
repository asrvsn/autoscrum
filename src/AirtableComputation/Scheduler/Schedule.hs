{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module AirtableComputation.Scheduler.Schedule 
  ( 
  -- * Scheduler types
    Schedule
  , ScheduleParams(..)
  , ScheduleSummary(..)
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
import           Data.List (minimumBy, sortBy)
import           Data.Monoid
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
  } deriving (Read, Show, Generic)
instance ToJSON ScheduleParams
instance FromJSON ScheduleParams

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

sampledScheduleSummary :: HasCallStack
                       => Int
                       -> ScheduleParams
                       -> TasksBase
                       -> IO ScheduleSummary
sampledScheduleSummary nSamples prms base = do
  let bn = tasksBayesNet thrTbl cntTbl 
  schedules <- forM [1..nSamples] $ \i -> do
    putStrLn $ "Sample " <> show i <> " ..."
    thrTbl_ <- sampleTable bn thrTbl
    let (blkTbl_, cntTbl_) = reconcileWithThreads thrTbl_ blkTbl cntTbl 
    let sched = schedule bn thrTbl_ blkTbl_ cntTbl_ devTbl tagTbl velTbl prms
    putStrLn $ "est. runtime: " <> show (getRuntime sched)
    return $! sched 

  let timedSchedules =  sortBy (compare `on` snd) $
                          zip schedules (map getRuntime schedules)
  let getConfidentSchedule c = case timedSchedules of 
                                  [] -> error "no schedules computed"
                                  (x:[]) -> fst x
                                  xs -> fst $ xs !! (max (round $ fromIntegral nSamples * c) 0)
  return ScheduleSummary 
    { sched20 = getConfidentSchedule 0.2
    , sched50 = getConfidentSchedule 0.5
    , sched80 = getConfidentSchedule 0.8
    }

-- Populates schedule with preassigned tasks.
initSchedule :: TasksBase -> Schedule
initSchedule base = 
  let getAssignee thr = case (threadAssignee thr) of 
        Nothing -> error $ "initSchedule: expected " ++ show thr ++ " to be assigned"
        Just d  -> d
      schedule0   = Map.fromList $ zip (map DevID devs) (repeat [])
      preAssigned = 
        catMaybes $ for (selectAll (threads base)) $ 
          \(thrId, thr) -> 
            case (threadStatus thr) of 
              Nothing -> Nothing
              Just s  -> 
                case s of 
                  WorkingOn   -> Just (thrId, getAssignee thr)
                  _           -> Nothing

  in foldl' (\s (t,d) -> assign base t d s) schedule0 preAssigned

schedule :: HasCallStack 
        => BayesNet RecordID
        -> TasksBase
        -> ScheduleParams
        -> Schedule
schedule bn base prms = 
  execState flushTasks (initSchedule base) 
  where
    devs  = selectAllKeys (developers base)

    prioritization = prioritize bn base

    flushTasks :: HasCallStack => State Schedule ()
    flushTasks = do
      ts <- getUnblockedTasks
      case ts of 
        [] -> return ()
        _  -> do
          let possibleAssignments = case [(t, DevID d) | t <- ts, d <- devs] of 
                                      [] -> error "flushTasks: got empty possibleAssignments"
                                      xs -> xs
          features <- mapM (uncurry assignmentFeatures) possibleAssignments
          let losses = map (loss prms) (rescaleFeatures features)
          let ((t, d), _) = minimumBy (compare `on` snd) (zip possibleAssignments losses)
          modify $ assign base t d
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
      let t_elapsed = taskCompletionTime base thrId devId   
      -- (3) task priority
      let priority = prioritization `lookup` thrId
      return $ Features {
          f_completed = t_unblocked + t_elapsed
        , f_priority = priority
        }

assign :: TasksBase -> ThreadID -> DevID -> Schedule -> Schedule 
assign base thrId devId mp = do
  let dt_task_  = taskCompletionTime base thrId devId
    -- TODO(anand) deal with negative completion times (bad data) better.
      dt_task   = if dt_task_ > 0 then dt_task_ else 1
      t_task    = getTaskAvailability base mp thrId 
      t_dev     = getDevAvailability mp devId
      elapsed   = if t_task > t_dev 
                    then [(t_task + dt_task, Just thrId), (t_task, Nothing)]
                    else [(t_dev + dt_task, Just thrId)]
  in  Map.adjust (\ts -> elapsed ++ ts) devId mp

getTaskAvailability :: HasCallStack => TasksBase -> Schedule -> ThreadID -> Double
getTaskAvailability base mp thrId = 
  let blkThrs   = getBlockingThreads (blocks base) (threads base) thrId
      blkTimes  = map (getCompletedTaskTime mp) blkThrs
  return $ case blkTimes of 
    [] -> 0
    _  -> assertPositive "blkTimes" blkTimes $ maximum blkTimes

getDevAvailability :: HasCallStack => Schedule -> DevID -> Double
getDevAvailability mp devId = 
  case mp `lookup` devId of
    []   -> 0
    t:ts -> assertPositive "devAvailability" mp $ fst t 

getCompletedTaskTime :: Schedule -> ThreadID -> Double
getCompletedTaskTime mp thrId = 
  let rec [] = error $ "could not find completed task " ++ show thrId ++ "in schedule"
      rec (timeline:rest) = case find (\(_, mode) -> Just thrId == mode) timeline of 
                              Just (t_task, _) -> t_task
                              Nothing -> rec rest
  in rec (Map.elems mp) 

assertPositive :: (HasCallStack, Show a) => String -> a -> Double -> Double
assertPositive tag a i = 
  if i > 0 
    then i 
    else error $ "assertPositive failed for {" <> tag <> "}: \n" <> show a

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
rescaleFeatures :: HasCallStack => [Features] -> [Features]
rescaleFeatures fs = map (f_unop fromNaN . (\x -> (x - x_min) / (x_max - x_min))) fs
  where
    fromNaN a = if isNaN a then 0 else a
    x_min = f_nop (minimumOr (error $ "infinite runtime: " <> show fs) . filter (not . isInfinite)) fs
    x_max = f_nop (maximumOr (error $ "infinite runtime: " <> show fs) . filter (not . isInfinite)) fs

-- * Schedule accessors

getRuntime :: HasCallStack => Schedule -> Double
getRuntime = maximumOr (error "schedule is empty") . map (headOr 0 . map fst) . Map.elems

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