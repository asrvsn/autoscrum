module Constants where

import Types

api_url :: String
api_url = "https://api.airtable.com/v0/appfGT5ACymqznlOP/"

api_key :: ByteString
api_key = "keyDSaC5Xv0BoKTSD"

master_thread_name :: String
master_thread_name = "Valuable at work"

sched_params :: ScheduleParams
sched_params = ScheduleParams {
    w_unblocked = 1.0
  , w_elapsed   = 1.0
  , w_priority  = 1.0
  }