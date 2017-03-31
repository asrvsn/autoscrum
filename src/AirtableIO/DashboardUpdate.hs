{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AirtableIO.DashboardUpdate 
  ( uploadTasksDiff
  , uploadEstimates
  , uploadGantt
  , uploadEstimateHistory
  , uploadComputedSchedule
  ) where

import           Data.Time.Clock (UTCTime)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Aeson
import           Data.Monoid
import           Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import           Data.Foldable (forM_)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Control.Monad (void)
import           System.Process (system)
import           Airtable.Table
import           Airtable.Query

import AirtableComputation.Scheduler.Schedule
import AirtableIO.TasksBase
import AirtableIO.DashboardBase
import FileIO


uploadTasksDiff :: UTCTime
                -> AirtableOptions
                -> Table Thread 
                -> Table Thread
                -> Table Developer
                -> IO ()
uploadTasksDiff curTime opts thrTbl_ thrTbl devTbl = 
  case cudHistory of 
    [] -> putStrLn "Change history is empty. Not uploading anything."
    _  -> 
      forM_ payloads $ \payload -> 
        createRecord opts "Task status changes" payload
  where
    CUDHistory cudHistory = tableCUDHistory (==) thrTbl_ thrTbl
    payloads = (flip map) cudHistory $ \case
      Created thr -> 
        let assignee = vSelect devTbl <$> threadAssignee thr
        in  object [ "Thread name" .= threadName thr
                   , "Assignee" .= fmap devName assignee
                   , "Story pts" .= threadStoryPts thr
                   , "Change" .= [String "Added"] 
                   , "Date recorded" .= curTime
                   ]
      Updated oldThr newThr -> 
        if (threadFinished oldThr == False) && (threadFinished newThr == True) 
          then 
            let assignee = vSelect devTbl <$> threadAssignee newThr
            in  object [ "Thread name" .= threadName newThr 
                       , "Assignee" .= fmap devName assignee
                       , "Story pts" .= threadStoryPts newThr
                       , "Change" .= [String "Done"]  
                       , "Date recorded" .= curTime
                       ]
          else 
            let assignee = vSelect devTbl <$> threadAssignee newThr
                diff = BLC.unpack . encode $ object ["old" .= oldThr, "new" .= newThr]
            in  object [ "Thread name" .= threadName newThr
                       , "Assignee" .= fmap devName assignee
                       , "Story pts" .= threadStoryPts newThr
                       , "Change" .= [String "Updated"] 
                       , "Date recorded" .= curTime
                       , "Diff" .= diff
                       ]
      Deleted thr -> 
        let assignee = vSelect devTbl <$> threadAssignee thr
            chgStatus = if threadFinished thr then "Done" else "Deleted"
        in  object [ "Thread name" .= threadName thr
                   , "Assignee" .= fmap devName assignee
                   , "Story pts" .= threadStoryPts thr
                   , "Change" .= [String chgStatus] 
                   , "Date recorded" .= curTime
                   ]

uploadEstimates :: UTCTime -> AirtableOptions -> ScheduleSummary -> ThreadName -> IO ()
uploadEstimates curTime opts summary (ThreadName thrName) = 
  void $ createRecord opts "Time estimations" $ 
    toJSON $ TimeEstimation 
      { runDate = curTime
      , estParentThread = thrName
      , est20 = getRuntime (sched20 summary)
      , est50 = getRuntime (sched50 summary)
      , est80 = getRuntime (sched80 summary) 
      }

uploadGantt :: UTCTime 
            -> AirtableOptions
            -> Table Thread
            -> Table Developer
            -> ThreadName
            -> Schedule
            -> IO ()
uploadGantt curTime opts thrTbl devTbl thrName sched = do
  persist "sched_vis" (schedule2vis thrTbl devTbl sched)
  system "python gantt.py sched_vis"
  ganttUrl <- readFile "sched_vis.url"
  void $ createRecord opts "Plot links" $ 
    object [ "Parent thread" .= thrName
           , "Link" .= ganttUrl
           , "Link type" .= [String "Median gantt chart"]
           , "Last updated" .= curTime
           ]

uploadEstimateHistory :: UTCTime -> AirtableOptions -> ThreadName -> IO ()
uploadEstimateHistory curTime opts (ThreadName thrName) = do
  timeEsts <- getRecords opts "Time estimations"
  let thrTimeEsts = filter (\est -> estParentThread est == thrName) $ vSelectAll timeEsts
  persist "estimates_over_time" (map toPyTimeEst (sortByTime thrTimeEsts))
  system $ "python estimates.py estimates_over_time " <> T.unpack thrName
  estUrl <- readFile "estimates_over_time.url"
  void $ createRecord opts "Plot links" $ 
    object [ "Link" .= estUrl
           , "Parent thread" .= thrName
           , "Link type" .= [String "Estimates over time"]
           , "Last updated" .= curTime
           ]
  where
    sortByTime = sortBy (compare `on` runDate)
    toPyTimeEst est = (runDate est, est20 est, est50 est, est80 est)

uploadComputedSchedule :: TasksBase -> UTCTime -> AirtableOptions -> ThreadName -> Schedule -> IO ()
uploadComputedSchedule base curTime opts thrName sched = 
  forM_ entries $ \(devId, timeline) -> 
    forM_ timeline $ \(elapsed, thrId) -> 
      let withThr def f = maybe def (f . vSelect (threads base)) thrId 
          preassigned   = withThr False (isJust . threadAssignee)
          task          = withThr (ThreadName "Blocked") threadName
      in 
        createRecord opts "Computed schedules" $ 
          object [ "Parent thread" .= thrName
                 , "Preassigned?" .= preassigned 
                 , "Task" .= task
                 , "Developer" .= devName (vSelect (developers base) devId)
                 , "Runtime" .= elapsed
                 , "Date computed" .= curTime
                 ]
  where
    entries = Map.toList (convertToDiffedRuntime sched)
