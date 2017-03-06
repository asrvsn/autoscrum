{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Types
import Data.Text (Text)
import Data.ByteString (ByteString)

app_id :: String
app_id = "appfGT5ACymqznlOP"

api_key :: String
api_key = "keyDSaC5Xv0BoKTSD"

master_thread_name :: ThreadName
master_thread_name = ThreadName "Valuable at work"

default_sched_params :: ScheduleParams
default_sched_params = ScheduleParams {
    w_completed = 1.0
  , w_priority  = 1.0
  }

cache_ext :: String
cache_ext = ".cache"

url_ext :: String
url_ext = ".url"

n_schedule_samples :: Int
n_schedule_samples = 30