{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Types
import Data.Text (Text)
import Data.ByteString (ByteString)

api_url :: String
api_url = "https://api.airtable.com/v0/appfGT5ACymqznlOP/"

api_key :: ByteString
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