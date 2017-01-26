{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import Types
import Constants

import System.Exit (exitSuccess)

opts :: [String]
opts = [
    "prioritize"
  , "schedule"
  , "train"
  ]

main :: IO ()
main = do
  putStrLn "===== ALPHASHEETS TASK SCHEDULER ====="

  cmdOptions [
      "prioritize"
    , "schedule"
    , "train"
    , "download"
    , "upload"
    , "exit"
    ] $ \case

      "prioritize" -> do
        thrTbl <- getTable "Threads"
        blkTbl <- getTable "Blocks" 
        cntTbl <- getTable "Containments" 

        let priorities = computePriorities thrTbl blkTbl cntTbl 
        persist "priorities" priorities
        putStrLn "...Done."

      "schedule" -> do
        thrTbl <- getTable "Threads"
        blkTbl <- getTable "Blocks" 
        cntTbl <- getTable "Containments" 
        devTbl <- getTable "Developers" 
        tagTbl <- getTable "Task Types" 
        velTbl <- getTable "Velocities" 

        prms <- ynCached "sched_params" (return default_sched_params)

        putStrLn "\nComputing schedule using parameters:\n"
        putStrLn $ debug prms
        putStrLn "...\n"
        let schedule = computeSchedule thrTbl blkTbl cntTbl devTbl tagTbl velTbl prms
        putStrLn $ debug (devTbl, schedule)
        persist "schedule" schedule
        putStrLn "\n...Done."
        
      "train" -> do
        error "not implemented"

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
              priorities <- retrieve "priorities" :: IO [Priority]
              mapM_ uploadPriority priorities 
              putStrLn "done."

      "exit" -> do
        putStrLn "===== EXITED ====="
        exitSuccess


