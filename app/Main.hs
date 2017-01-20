module Main where

import Lib
import Types
import Constants

main :: IO ()
main = do
  putStrLn "===== ALPHASHEETS TASK SCHEDULER ====="
  putStrLn "..."

  thrTbl <- getTable "Threads" :: Table Thread
  blkTbl <- getTable "Blocks" :: Table Block
  cntTbl <- getTable "Containments" :: Table Containment
  devTbl <- getTable "Developers" :: Table Developersoper
  tagTbl <- getTable "Task Types" :: Table Tag
  velTbl <- getTable "Velocities" :: Table Velocity
  putStrLn "Tables downloaded."

  putStrLn "Computing priorities..."
  priorities <- computePriorities thrTbl blkTbl cntTbl 
  putStrLn "...Done."
  putStrLn "PRIORITIES:"
  putStrLn $ debug (thrTbl, priorities)

  yn "Upload priorities?"
     (mapM_ uploadPriority priorities >> putStrLn "...Uploaded.")
     (putStrLn "Upload canceled.")

  putStrLn "Computing schedule using parameters:\n"
  putStrLn $ debug sched_params
  putStrLn "\n..."
  schedule <- computeSchedule thrTbl blkTbl cntTbl devTbl tagTbl velTbl sched_params
  putStrLn "...Done."
  putStrLn $ debug (devTbl, schedule)

  yn "Upload schedule?"
      (uploadSchedule schedule >> putStrLn "...Uploaded.")
      (putStrLn "Upload canceled.")

  putStrLn "===== DONE ====="

yn :: String -> IO () -> IO () -> IO ()
yn ask y n = do
  putStrLn $ ask ++ " (Y/N)"
  resp <- getLine
  case resp of 
    "y" -> y
    "Y" -> y
    "n" -> n
    "N" -> n
    _   -> yn ask y n
