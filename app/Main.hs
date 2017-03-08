{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import Types
import Missing
import Constants

import System.Process (system)
import Data.Time.Clock (getCurrentTime)

import Airtable.Table
import Airtable.Query

opts :: AirtableOptions
opts = AirtableOptions { apiKey = api_key
                       , appId = app_id
                       , apiVersion = 0
                       }

dashOpts :: AirtableOptions 
dashOpts = opts { appId = dash_app_id }
                       
main :: IO ()
main = do
  putStrLn "\n===== ALPHASHEETS TASK SCHEDULER ====="


  cmdOptions [
      "first-time setup (run this if you haven't)"
    , "update dashboard"
    , "lookup record"
    ] $ \case

      "first-time setup (run this if you haven't)" -> do
        mightNeedSudo "pip install plotly shortid"
        system "mkdir ~/.plotly"
        system "cp .plotly/.credentials ~/.plotly/"
        return ()

      "update dashboard" -> do
        -- (-1) get all relevant tables
        thrTbl <- getRecords opts "Threads"
        blkTbl <- getRecords opts "Blocks" 
        devTbl <- getRecords opts "Developers" 
        cntTbl <- getRecords opts "Containments" 
        tagTbl <- getRecords opts "Task Types" 
        velTbl <- getRecords opts "Velocities" 

        -- (0) get current time
        curTime <- getCurrentTime

        -- (1) upload diff in threads table
        thrTbl' <- retrieve "Threads_table_old"
        let CUDRecord cudRecord = tableCUDRecord (==) thrTbl' thrTbl
        forM_ cudRecord $ \case
          Created thr -> do
            let asignee = select devTbl <$> threadAsignee thr
            createRecord dashOpts "Task status changes" $ 
              object [ "Thread name" .= threadName thr
                     , "Asignee" .= asignee
                     , "Story pts" .= threadStoryPts thr
                     , "Change" .= "Added"
                     , "Date recorded" .= curTime
                     ]
          Updated oldThr newThr -> 
            if (threadFinished oldThr == False) && (threadFinished newThr == True) 
              then do
                let asignee = select devTbl <$> threadName newThr
                createRecord dashOpts "Task status changes" $ 
                  object [ "Thread name" .= threadName newThr 
                         , "Asignee" .= asignee
                         , "Story pts" .= threadStoryPts newThr
                         , "Change" .= "Done" 
                         , "Date recorded" .= curTime
                         ]
              else do
                let asignee = select devTbl <$> threadName newThr
                let diff = object ["old" .= oldThr, "new" .= newThr]
                createRecord dashOpts "Tasks status changes" $
                  object [ "Thread name" .= threadName newThr
                         , "Asignee" .= asignee
                         , "Story pts" .= threadStoryPts newThr
                         , "Change" .= "Updated"
                         , "Date recorded" .= curTime
                         , "Diff" .= diff
                         ]
          Deleted thr -> do
            let assignee = select devTbl <$> threadAssignee thr
            createRecord dashOpts "Task status changes" $ 
              object [ "Thread name" .= threadName thr
                     , "Asignee" .= asignee
                     , "Story pts" .= threadStoryPts thr
                     , "Change" .= "Deleted"
                     , "Date recorded" .= curTime
                     ]

        -- (2) persist new threads table
        persist "Threads_table_old" thrTbl

        -- (3) compute 20%, 50%, 80% schedules 
        prms <- ynCached "sched_params" $ 
          yn  "use default parameters?" 
              (return default_sched_params) 
              enterParameters

        putStrLn "\nComputing schedule using parameters:\n"
        putStrLn $ debug prms
        putStrLn "...\n"

        let bn = bayesNet thrTbl cntTbl 
        schedules <- forM [1..n_schedule_samples] $ \i -> do
          putStrLn $ "Sample " <> show i <> " ..."
          thrTbl_ <- taskSample bn thrTbl
          let (blkTbl_, cntTbl_) = reconcileWithThreads thrTbl_ blkTbl cntTbl 
          return $ schedule bn thrTbl_ blkTb_ cntTbl_ devTbl tagTbl velTbl prms

        let timedSchedules =  sortBy (compare `on` snd) $
                                zip schedules (map getRuntime schedules)
        let sched20 = timedSchedules !! (round $ n_schedule_samples * 0.2)
        let sched50 = timedSchedules !! (round $ n_schedule_samples * 0.5)
        let sched80 = timedSchedules !! (round $ n_schedule_samples * 0.8)

        putStrLn "\nMedian schedule:"
        putStrLn $ debug (devTbl, sched50)

        -- (4) upload estimates
        createRecord dashOpts "Time estimations" $ 
          toJSON $ TimeEstimation curTime
                                  getRuntime sched20
                                  getRuntime sched50
                                  getRuntime sched80

        -- (5) upload gantt chart
        persist "sched50_vis" (schedule2vis thrTbl devTbl sched50)
        system "python gantt.py sched50_vis"
        ganttUrl <- readFile "sched50_vis.url"
        createRecord dashOpts "Plot links" $ 
          object [ "Link" .= ganttUrl
                 , "Link type" .= "Median gantt chart"
                 ]

        -- (6) upload estimates over time chart
        timeEsts <- getRecords dashOpts "Time estimations"
        let sortedTimeEsts = sortBy (compare `on` runDate) timeEsts
        let toPyTimeEst est = [est20 est, est50 est, est80 est]
        persist "estimates_over_time" (map toPyTimeEst sortedTimeEsts)
        system "python estimates.py estimates_over_time"
        estUrl <- readFile "estimates_over_time.url"
        createRecord dashOpts "Plot links" $ 
          object [ "Link" .= estUrl
                 , "Link type" .= "Estimates over time"
                 ]

      "lookup record" -> do
        cmdOptions [
            "threads"
          , "blocks"
          , "containments"
          , "developers"
          , "task types"
          , "velocities"
          ] $ \case
            "threads" -> do
              putStrLn "Enter record from Threads table:"
              resp <- getLine
              thrTbl <- getRecords opts "Threads" :: IO (Table Thread)
              putStrLn . show $ selectMaybe thrTbl resp
            "blocks" -> do
              putStrLn "Enter record from Blocks table:"
              resp <- getLine
              blkTbl <- getRecords opts "Blocks" :: IO (Table Block)
              putStrLn . show $ selectMaybe blkTbl resp
            "containments" -> do
              putStrLn "Enter record from Containments table:"
              resp <- getLine
              cntTbl <- getRecords opts "Containments" :: IO (Table Containment)
              putStrLn . show $ selectMaybe cntTbl resp
            "developers" -> do
              putStrLn "Enter record from Developers table:"
              resp <- getLine
              devTbl <- getRecords opts "Developers" :: IO (Table Developer)
              putStrLn . show $ selectMaybe devTbl resp
            "task types" -> do
              putStrLn "Enter record from Task Types table:"
              resp <- getLine
              tagTbl <- getRecords opts "Task Types" :: IO (Table Tag)
              putStrLn . show $ selectMaybe tagTbl resp
            "velocities" -> do
              putStrLn "Enter record from Velocities table:"
              resp <- getLine
              velTbl <- getRecords opts "Velocities" :: IO (Table Velocity)
              putStrLn . show $ selectMaybe velTbl resp

  putStrLn "===== EXITED ====="
