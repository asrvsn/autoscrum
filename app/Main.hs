{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import Types
import Missing
import Constants

import System.Process (system)

main :: IO ()
main = do
  putStrLn "\n===== ALPHASHEETS TASK SCHEDULER ====="

  cmdOptions [
      "first-time setup"
    , "prioritize"
    , "schedule"
    , "download"
    , "upload"
    , "visualize schedule"
    , "lookup record"
    ] $ \case

      "first-time setup" -> do
        mightNeedSudo "pip install plotly shortid"
        system "mkdir ~/.plotly"
        system "cp .plotly/.credentials ~/.plotly/"
        return ()

      "prioritize" -> do
        thrTbl <- getTable "Threads"
        blkTbl <- getTable "Blocks" 
        cntTbl <- getTable "Containments" 

        let prioritization = computePrioritization thrTbl blkTbl cntTbl 
        persist "prioritization" prioritization
        putStrLn "...Done."

      "schedule" -> do
        thrTbl <- getTable "Threads"
        blkTbl <- getTable "Blocks" 
        cntTbl <- getTable "Containments" 
        devTbl <- getTable "Developers" 
        tagTbl <- getTable "Task Types" 
        velTbl <- getTable "Velocities" 

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

      "download" -> do
        getTable "Threads" :: IO (Table Thread)
        getTable "Blocks" :: IO (Table Block)
        getTable "Containments" :: IO (Table Containment)
        getTable "Developers" :: IO (Table Developer)
        getTable "Task Types" :: IO (Table Tag)
        getTable "Velocities" :: IO (Table Velocity)
        return ()

      "upload" -> do
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
              thrTbl <- getTable "Threads" :: IO (Table Thread)
              putStrLn . show $ selectMaybe thrTbl resp
            "blocks" -> do
              putStrLn "Enter record from Blocks table:"
              resp <- getLine
              blkTbl <- getTable "Blocks" :: IO (Table Block)
              putStrLn . show $ selectMaybe blkTbl resp
            "containments" -> do
              putStrLn "Enter record from Containments table:"
              resp <- getLine
              cntTbl <- getTable "Containments" :: IO (Table Containment)
              putStrLn . show $ selectMaybe cntTbl resp
            "developers" -> do
              putStrLn "Enter record from Developers table:"
              resp <- getLine
              devTbl <- getTable "Developers" :: IO (Table Developer)
              putStrLn . show $ selectMaybe devTbl resp
            "task types" -> do
              putStrLn "Enter record from Task Types table:"
              resp <- getLine
              tagTbl <- getTable "Task Types" :: IO (Table Tag)
              putStrLn . show $ selectMaybe tagTbl resp
            "velocities" -> do
              putStrLn "Enter record from Velocities table:"
              resp <- getLine
              velTbl <- getTable "Velocities" :: IO (Table Velocity)
              putStrLn . show $ selectMaybe velTbl resp

  putStrLn "===== EXITED ====="
