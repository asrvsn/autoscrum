{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AirtableIO.DashboardUpdate 
  ( uploadTasksDiff
  , uploadEstimates
  , uploadGantt
  , uploadEstimateHistory
  ) where

import           Data.Time.Clock (UTCTime)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Aeson
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
        in  object [ "Thread name" .= threadName thr
                   , "Assignee" .= fmap devName assignee
                   , "Story pts" .= threadStoryPts thr
                   , "Change" .= [String "Deleted"] 
                   , "Date recorded" .= curTime
                   ]

uploadEstimates :: UTCTime -> AirtableOptions -> ScheduleSummary -> IO ()
uploadEstimates curTime opts summary = 
  void $ createRecord opts "Time estimations" $ 
    toJSON $ TimeEstimation 
      { runDate = curTime
      , est20 = getRuntime (sched20 summary)
      , est50 = getRuntime (sched50 summary)
      , est80 = getRuntime (sched80 summary) 
      }

uploadGantt :: UTCTime 
            -> AirtableOptions
            -> Table Thread
            -> Table Developer
            -> Schedule
            -> IO ()
uploadGantt curTime opts thrTbl devTbl sched = do
  persist "sched_vis" (schedule2vis thrTbl devTbl sched)
  system "python gantt.py sched_vis"
  ganttUrl <- readFile "sched_vis.url"
  void $ createRecord opts "Plot links" $ 
    object [ "Link" .= ganttUrl
           , "Link type" .= ("Median gantt chart" :: String)
           , "Last updated" .= curTime
           ]

uploadEstimateHistory :: UTCTime -> AirtableOptions -> IO ()
uploadEstimateHistory curTime opts = do
  timeEsts <- getRecords opts "Time estimations"
  persist "estimates_over_time" (map toPyTimeEst (sortByTime $ vSelectAll timeEsts))
  system "python estimates.py estimates_over_time"
  estUrl <- readFile "estimates_over_time.url"
  void $ createRecord opts "Plot links" $ 
    object [ "Link" .= estUrl
           , "Link type" .= ("Estimates over time" :: String)
           , "Last updated" .= curTime
           ]
  where
    sortByTime = sortBy (compare `on` runDate)
    toPyTimeEst est = (runDate est, est20 est, est50 est, est80 est)