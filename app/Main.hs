{-# LANGUAGE LambdaCase #-}

module Main where

import Constants
import AirtableComputation.Scheduler.Schedule 
import AirtableIO.TasksBase
import AirtableIO.TasksBaseValidate
import AirtableIO.DashboardUpdate
import FileIO
import Missing

import System.Process (system)
import Data.Time.Clock (getCurrentTime)
import Data.Monoid
import Airtable.Table
import Airtable.Query
import Text.Read (readMaybe)
import qualified Data.Text as T

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
        mightNeedSudo "pip install plotly shortid numpy"
        system "mkdir ~/.plotly"
        system "cp .plotly/.credentials ~/.plotly/"
        return ()

      "update dashboard" -> do
        -- (-2) get all relevant tables
        putStrLn "[0] Getting all tables"
        base0 <- getTasksBase opts
        putStrLn "Got all tables"

        -- (-1) data validation
        let thrGetter = do  putStrLn "Enter thread name for schedule computation"
                            parentThrName <- getLine
                            case findThr (threads base0) (ThreadName $ T.pack parentThrName) of  
                              Just parentThrId -> return parentThrId
                              Nothing -> do
                                putStrLn $ "Couldn't find thread ID for name: " ++ show parentThrName
                                thrGetter 
        parentThrId <- thrGetter
        let base1 = selectDescendantsOf parentThrId base0

        -- (0) get current time
        putStrLn "[2] get current time"
        curTime <- getCurrentTime

        putStrLn "[1] Data validation"
        base2 <- runValidator base1

        -- (1) upload diff in threads table
        let uploadDiff = do putStrLn "[3] upload diff in threads table"
                            oldThrTbl <- retrieveRemote "Threads_table_old" 
                            case oldThrTbl of
                              Left l -> do
                                putStrLn l
                                putStrLn "Not uploading task status changes."
                              Right r -> 
                                uploadTasksDiff curTime dashOpts r (threads base0) (developers base0)
        yn "Upload diff?" uploadDiff (putStrLn "not uploading.")

        -- (2) persist new threads table
        putStrLn "[4] persist new threads table"
        persistRemote "Threads_table_old" (threads base0)

        -- (3) compute 20%, 50%, 80% schedules 
        putStrLn "[5] compute schedule estimations"
        prms <- ynCached "sched_params" $ 
          yn  "use default parameters?" 
              (return default_sched_params) 
              enterParameters

        yn ("use default (" ++ show est_fudge_factor ++ ") fudge factor?")
           (return ())
           (putStrLn "Change it in Constants.hs, and recompile" >> abort)

        putStrLn "\nComputing schedule using parameters:\n"
        putStrLn $ debug prms
        putStrLn "...\n"
        
        schedSummary <- sampledScheduleSummary n_schedule_samples prms base2

        putStrLn "\nMedian schedule:"
        putStrLn $ debug (developers base2, sched50 schedSummary)

        -- (4) upload estimates
        putStrLn "[6] upload estimates"
        let parentThrName = threadName $ vSelect (threads base0) parentThrId
        uploadEstimates curTime dashOpts schedSummary parentThrName

        -- (5) upload gantt chart
        putStrLn "[7] upload gantt chart"
        uploadGantt curTime dashOpts (threads base2) (developers base2) parentThrName (sched50 schedSummary)

        -- (6) upload estimates over time chart
        putStrLn "[8] upload estimates over time chart"
        uploadEstimateHistory curTime dashOpts parentThrName

        -- (7) upload computed schedule
        putStrLn "[9] upload computed schedule"
        uploadComputedSchedule base0 curTime dashOpts parentThrName (sched50 schedSummary)

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
            _ -> putStrLn "That's not an option"

      _ -> putStrLn "That's not an option"

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