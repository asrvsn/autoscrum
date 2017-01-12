{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Types

import System.Random

import Data.Monoid

-- API methods

api_key :: ByteString
api_key = "keyDSaC5Xv0BoKTSD"

getTable :: (FromJSON (Table a)) => String -> IO (Table a)
getTable tblStr = getTableParts opts url
  where
    opts = defaults & header "Authorization" .~ [api_key] 
                    & param "view" .~ ["Main View"]
    url  = "https://api.airtable.com/v0/appfGT5ACymqznlOP/" <> tblStr

getTableParts :: Options -> String -> IO (Table a)
getTableParts opts url = do
  resp <- getWith opts url
  getMore (fromResp resp)
  where
    getMore tbl = case tableOffset tbl of 
      Just offset -> do
        resp <- getWith (opts & param "offset" .~ [offset]) url
        getMore $ tbl <> fromResp resp
      Nothing -> 
        pure tbl

fromResp :: Response ByteString -> Table a
fromResp r = decode $ r ^. responseBody

-- Table methods

select :: Table a -> RecordID -> a
select tbl rec = tableRecords tbl Map.! rec

selectWhere :: Table a -> (RecordID -> a -> Bool) -> [a]
selectWhere tbl f = filter (uncurry f) (toList tbl)

foreign :: (a -> RecordID) -> Table a -> Table b -> RecordID -> b
foreign key tblA tblB rec = select tblB $ key (select tblA rec)

-- Specific methods

devTags :: Table Velocity
        -> Developer
        -> [TagID] 
devTags velTbl = 
  map (TagID . vTag . select velTbl) . devVelocities 

-- Computation

taskCompletionTime :: Table Tag
                   -> Table Thread
                   -> Table Developer
                   -> Table Velocity
                   -> ThreadID 
                   -> DevID 
                   -> Double
taskCompletionTime tagTbl thrTbl devTbl velTbl (ThreadID thrId) (DevID devId) = 
  storyPts / product devMultipliers * product missingMultipliers
  where
    dev = select devTbl devId
    thr = select thrTbl thrId
    storyPts = threadStoryPts (select thrTbl thrId)
    devMultipliers = map vMultiplier $ 
      selectWhere velTbl $ \_ v -> 
            vTag v `elem` threadTags thr 
        &&  vDeveloper v == devId
    missingMultipliers = map tagMultiplierIfMissing $ 
      selectWhere tagTbl $ \rec _ ->
            rec `elem` threadTags thr
        &&  TagID rec `notElem` devTags velTbl dev

blockageFactor :: Table Thread
               -> Table Block
               -> ThreadID
               -> Double
blockageFactor thrTbl blkTbl (ThreadID thrId) = 
  product $ map (\b -> 1 / (1 - b)) blockages
  where
    blockages = map getBlockage $ 
      selectWhere blkTbl $ \_ blk -> 
        blockedThread blk == thrId
    getBlockage blk = 
      let blkThr = select thrTbl (blockingThread blk) 
      in  if assignable blkThr
            then blockingPercentage blk
            else 100 -- TODO(anand) what should this actually be

taskSample :: Table Thread
           -> Table Containment
           -> ThreadID 
           -> IO [ThreadID]
taskSample thrTbl cntTbl thrId = 
  probabilisticInclusion $ containmentDistribution thrTbl cntTbl thrId

containmentDistribution :: Table Thread
                        -> Table Containment
                        -> ThreadID
                        -> [(Double, ThreadID)]
containmentDistribution = undefined -- TODO(anand) are the paths guaranteed to be unique

probabilisticInclusion :: [(Double, a)] -> IO [a]
probabilisticInclusion pdf = catMaybes <$> mapM event pdf
  where
    event (p, a) = do
      outcome <- unfairCoin p
      return $ 
        if outcome then Just a else Nothing 

unfairCoin :: Double -> IO Bool
unfairCoin p = flip <$> getStdRandom (randomR (1,100))
  where
    flip r = r < (p * 100) 

-- Steps

data Assignment = Assignment {
    assignedTask :: ThreadID
  , assignedDev :: DevID
  }

step1 :: Table Tag
      -> Table Thread
      -> Table Developer
      -> Table Velocity
      -> Table Containment
      -> ThreadID
      -> [Assignment]
step1 tagTbl thrTbl devTbl velTbl cntTbl thrId = undefined -- blocked
