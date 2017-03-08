module AirtableIO.DashboardUpdate 
  ( uploadTasksDiff
  , uploadEstimates
  , uploadGantt
  , uploadEstimateHistory
  ) where


uploadTasksDiff :: UTCTime
                -> AirtableOptions
                -> Table Thread 
                -> Table Thread
                -> Table Developer
                -> IO ()
uploadTasksDiff opts thrTbl_ thrTbl devTbl = 
  forM_ payloads $ \payload -> 
    createRecord opts "Task status changes" payload
  where
    CUDRecord cudRecord = tableCUDRecord (==) thrTbl_ thrTbl
    payloads = for cudRecord $ \case
      Created thr -> 
        let asignee = vSelect devTbl <$> threadAsignee thr
        in  object [ "Thread name" .= threadName thr
                   , "Asignee" .= asignee
                   , "Story pts" .= threadStoryPts thr
                   , "Change" .= "Added"
                   , "Date recorded" .= curTime
                   ]
      Updated oldThr newThr -> 
        if (threadFinished oldThr == False) && (threadFinished newThr == True) 
          then 
            let asignee = vSelect devTbl <$> threadName newThr
            in  object [ "Thread name" .= threadName newThr 
                       , "Asignee" .= asignee
                       , "Story pts" .= threadStoryPts newThr
                       , "Change" .= "Done" 
                       , "Date recorded" .= curTime
                       ]
          else 
            let asignee = vSelect devTbl <$> threadName newThr
                diff = object ["old" .= oldThr, "new" .= newThr]
            in  object [ "Thread name" .= threadName newThr
                       , "Asignee" .= asignee
                       , "Story pts" .= threadStoryPts newThr
                       , "Change" .= "Updated"
                       , "Date recorded" .= curTime
                       , "Diff" .= diff
                       ]
      Deleted thr -> 
        let assignee = vSelect devTbl <$> threadAssignee thr
        in  object [ "Thread name" .= threadName thr
                   , "Asignee" .= asignee
                   , "Story pts" .= threadStoryPts thr
                   , "Change" .= "Deleted"
                   , "Date recorded" .= curTime
                   ]

uploadEstimates :: UTCTime -> AirtableOptions -> ScheduleSummary -> IO ()
uploadEstimates curTime opts summary = 
  createRecord opts "Time estimations" $ 
    toJSON $ TimeEstimation curTime
                            getRuntime (sched20 summary)
                            getRuntime (sched50 summary)
                            getRuntime (sched80 summary) 

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
  createRecord dashOpts "Plot links" $ 
    object [ "Link" .= ganttUrl
           , "Link type" .= "Median gantt chart"
           , "Last updated" .= curTime
           ]

uploadEstimateHistory :: UTCTime -> AirtableOptions -> IO ()
uploadEstimateHistory curTime opts = do
  timeEsts <- getRecords dashOpts "Time estimations"
  persist "estimates_over_time" (map toPyTimeEst (sortByTime timeEsts))
  system "python estimates.py estimates_over_time"
  estUrl <- readFile "estimates_over_time.url"
  createRecord opts "Plot links" $ 
    object [ "Link" .= estUrl
           , "Link type" .= "Estimates over time"
           , "Last updated" .= curTime
           ]
  where
    sortByTime = sortBy (compare `on` runDate)
    toPyTimeEst est = [est20 est, est50 est, est80 est]