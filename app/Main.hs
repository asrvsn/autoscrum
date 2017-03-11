{-# LANGUAGE LambdaCase #-}

module Main where

import Constants
import AirtableComputation.Scheduler.Schedule 
import AirtableComputation.Tasks
import AirtableIO.TasksBase
import AirtableIO.DashboardUpdate
import FileIO
import Missing

import System.Process (system)
import Data.Time.Clock (getCurrentTime)
import Data.Monoid
import Airtable.Table
import Airtable.Query
import Text.Read (readMaybe)

default_sched_params :: ScheduleParams
default_sched_params = ScheduleParams {
    w_completed = 1.0
  , w_priority  = 1.0
  }

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
        -- (-2) get all relevant tables
        putStrLn "[0] Getting all tables"
        thrTbl <- getRecords opts "Threads"
        putStrLn "Got Threads"
        blkTbl <- getRecords opts "Blocks" 
        putStrLn "Got Blocks"
        devTbl <- getRecords opts "Developers" 
        putStrLn "Got Developers"
        cntTbl <- getRecords opts "Containments" 
        putStrLn "Got Containments"
        tagTbl <- getRecords opts "Task types" 
        putStrLn "Got Tasks Types"
        velTbl <- getRecords opts "Velocities" 
        putStrLn "Got Velocities"

        -- (-1) data validation
        putStrLn "[1] Data validation"
        putStrLn "(a) Check that for every thread there exists a developer with non-infinite completion time."
        case getImpossibleThreads thrTbl velTbl tagTbl devTbl of
          [] -> pure ()
          xs -> do
            putStrLn "The following have impossible tag configurations:"
            let thrNames = map (threadName . vSelect thrTbl) xs
            mapM_ (putStrLn . show) thrNames
            abort

        -- (0) get current time
        putStrLn "[2] get current time"
        curTime <- getCurrentTime

        -- (1) upload diff in threads table
        putStrLn "[3] upload diff in threads table"
        oldThrTbl <- retrieveRemote "Threads_table_old" 
        case oldThrTbl of
          Left l -> do
            putStrLn l
            putStrLn "Not uploading task status changes."
          Right r -> 
            uploadTasksDiff curTime dashOpts r thrTbl devTbl

        -- (2) persist new threads table
        putStrLn "[4] persist new threads table"
        persistRemote "Threads_table_old" thrTbl

        -- (3) compute 20%, 50%, 80% schedules 
        putStrLn "[5] compute 20%, 50%, 80% schedules"
        prms <- ynCached "sched_params" $ 
          yn  "use default parameters?" 
              (return default_sched_params) 
              enterParameters

        putStrLn "\nComputing schedule using parameters:\n"
        putStrLn $ debug prms
        putStrLn "...\n"

        schedSummary <- sampledScheduleSummary n_schedule_samples prms thrTbl blkTbl cntTbl devTbl tagTbl velTbl 

        putStrLn "\nMedian schedule:"
        putStrLn $ debug (devTbl, sched50 schedSummary)

        -- (4) upload estimates
        putStrLn "[6] upload estimates"
        uploadEstimates curTime dashOpts schedSummary

        -- (5) upload gantt chart
        putStrLn "[7] upload gantt chart"
        uploadGantt curTime dashOpts thrTbl devTbl (sched50 schedSummary)

        -- (6) upload estimates over time chart
        putStrLn "[8] upload estimates over time chart"
        uploadEstimateHistory curTime dashOpts

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

enterParameters :: IO ScheduleParams 
enterParameters = 
  ScheduleParams <$> reqDouble "w_completed"
                 <*> reqDouble "w_priority"
  where
    reqDouble s = do
      putStrLn $ "Enter " <> s
      resp <- getLine
      case readMaybe resp :: Maybe Double of 
        Just d -> return d
        Nothing -> reqDouble s

abort :: a
abort = error "===== ABORTED ======="