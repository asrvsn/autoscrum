{-# LANGUAGE OverloadedStrings #-}

module Constants where

-- | Source Airtable Base from which to pull data (tasks, developers, etc.)
app_id :: String
app_id = undefined

-- | Destination Airtable Base to send schedule computation to
dash_app_id :: String
dash_app_id = undefined

-- | Airtable API key
api_key :: String
api_key = undefined

cache_ext :: String
cache_ext = ".cache"

url_ext :: String
url_ext = ".url"

n_schedule_samples :: Int
n_schedule_samples = 1

est_fudge_factor :: Double
est_fudge_factor = 3.0