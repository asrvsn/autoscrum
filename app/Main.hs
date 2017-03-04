{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import Types
import Missing
import Constants

import System.Process (system)

import Airtable.Table
import Airtable.Query

opts :: AirtableOptions
opts = AirtableOptions { apiKey = api_key
                       , appId = app_id
                       , apiVersion = 0
                       }
                       
main :: IO ()
main = do
  putStrLn "\n===== ALPHASHEETS TASK SCHEDULER ====="


  cmdOptions [
      "first-time setup (run this if you haven't)"
    , "prioritize"
    , "schedule"
    , "download..."
    , "upload..."
    , "visualize schedule"
    , "lookup record"
    ] $ \case

      "first-time setup (run this if you haven't)" -> do
        mightNeedSudo "pip install plotly shortid"
        system "mkdir ~/.plotly"
        system "cp .plotly/.credentials ~/.plotly/"
        return ()

      "prioritize" -> do
        thrTbl <- getTable opts "Threads"
        blkTbl <- getTable opts "Blocks" 
        cntTbl <- getTable opts "Containments" 

        let prioritization = computePrioritization thrTbl blkTbl cntTbl 
        persist "prioritization" prioritization
        putStrLn "...Done."

      "schedule" -> do
        thrTbl <- getTable opts "Threads"
        blkTbl <- getTable opts "Blocks" 
        cntTbl <- getTable opts "Containments" 
        devTbl <- getTable opts "Developers" 
        tagTbl <- getTable opts "Task Types" 
        velTbl <- getTable opts "Velocities" 

        prms <- ynCached "sched_params" $ 
          yn  "use default parameters?" 
              (return default_sched_params) 
              enterParameters

        putStrLn "\nComputing schedule using parameters:\n"
        putStrLn $ debug prms
        putStrLn "...\n"
        let schedule = computeSchedule thrTbl blkTbl cntTbl devTbl tagTbl velTbl prms
        putStrLn $ debug (devTbl, schedule)
        persist "schedule" schedule
        putStrLn "\n...Done."
        
        persist "schedule_vis" (schedule2vis thrTbl devTbl schedule)

        yn "see visualization?" visualizeSchedule (pure ())

      "download..." -> do
        getTable opts "Threads" :: IO (Table Thread)
        getTable opts "Blocks" :: IO (Table Block)
        getTable opts "Containments" :: IO (Table Containment)
        getTable opts "Developers" :: IO (Table Developer)
        getTable opts "Task Types" :: IO (Table Tag)
        getTable opts "Velocities" :: IO (Table Velocity)
        return ()

      "upload..." -> do
        cmdOptions [
            "schedule"
          , "priorities"
          ] $ \case 
            "schedule" -> do
              schedule <- retrieve "schedule" :: IO Schedule
              uploadSchedule schedule 
              putStrLn "done."
            "priorities" -> do
              prioritization <- retrieve "prioritization" :: IO Prioritization
              uploadPrioritization prioritization 
              putStrLn "done."

      "visualize schedule" -> visualizeSchedule

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
              thrTbl <- getTable opts "Threads" :: IO (Table Thread)
              putStrLn . show $ selectMaybe thrTbl resp
            "blocks" -> do
              putStrLn "Enter record from Blocks table:"
              resp <- getLine
              blkTbl <- getTable opts "Blocks" :: IO (Table Block)
              putStrLn . show $ selectMaybe blkTbl resp
            "containments" -> do
              putStrLn "Enter record from Containments table:"
              resp <- getLine
              cntTbl <- getTable opts "Containments" :: IO (Table Containment)
              putStrLn . show $ selectMaybe cntTbl resp
            "developers" -> do
              putStrLn "Enter record from Developers table:"
              resp <- getLine
              devTbl <- getTable opts "Developers" :: IO (Table Developer)
              putStrLn . show $ selectMaybe devTbl resp
            "task types" -> do
              putStrLn "Enter record from Task Types table:"
              resp <- getLine
              tagTbl <- getTable opts "Task Types" :: IO (Table Tag)
              putStrLn . show $ selectMaybe tagTbl resp
            "velocities" -> do
              putStrLn "Enter record from Velocities table:"
              resp <- getLine
              velTbl <- getTable opts "Velocities" :: IO (Table Velocity)
              putStrLn . show $ selectMaybe velTbl resp

  putStrLn "===== EXITED ====="
