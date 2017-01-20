{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import qualified Data.HashMap.Strict as Map
import           Data.Aeson
import           Data.Text (Text)
import           Data.Foldable
import           Data.Hashable


-- Core types

newtype RecordID = RecordID Text deriving (FromJSON, Show, Eq)
instance Hashable RecordID

data Table a = Table {
    tableRecords :: HashMap RecordID a
  , tableOffset :: Maybe Text
  } deriving (Show)

instance (FromJSON a) => FromJSON (Table a) where
  parseJSON (Object v) = do
    recs <- v .: "records" :: Parser [Value]
    Table <$> foldlM parseRec Map.empty recs
    where
      parseRec tbl (Object v) = do
        recId <- v .: "id"
        obj <- v .: "fields" :: Parser a
        return $ Map.insert recId obj tbl

instance Monoid (Table a) where
  mempty = Table mempty Nothing
  mconcat (Table t1) (Table t1) = Table (mconcat t1 t2)

toList :: Table a -> [(RecordID, a)]
toList = Map.toList . tableRecords

-- ID types

newtype ThreadID = ThreadID {getThreadId :: RecordID} deriving (Show, Eq)
newtype DevID = DevID {getDevId :: RecordID} deriving (Show, Eq)
newtype TagID = TagID {getTagId :: RecordID} deriving (Show, Eq)

-- API types

data Tag = Tag {
    tagName :: Text
  , tagMultiplierIfMissing :: Double
  }

instance FromJSON Tag where
  parseJSON (Object v) = 
    Tag <$> v .: "Name" 
        <*> v .: "Multiplier if missing"

newtype ThreadName = ThreadName Text deriving (FromJSON)

data Thread = Thread {
    threadName :: ThreadName
  , threadTags :: [RecordID]
  , threadContainments :: [RecordID]
  , threadBlocks :: [RecordID]
  , threadStoryPts :: Double
  , threadAssignable :: Bool
  , threadFinished :: Bool
  }

instance FromJSON Thread where 
  parseJSON (Object v) = 
    Thread <$> v .: "Thread name"
           <*> v .: "Contained in"
           <*> v .: "Blocks"
           <*> v .:? "Story pts" .!= 42 -- what the hell should this default be 
           <*> (boolField <$> v .: "Assignable?")
           <*> (boolField <$> v .: "Done?")
    where
      boolField = \case
        Number n -> n == fromInteger 1
        _ -> error "boolean field is non-number"

instance Debug Thread where 
  debug = show . threadName

data Containment = Containment {
    parentThread :: RecordID
  , childThread :: RecordID
  , containmentProbability :: Double -- [0,1]
  }

instance FromJSON Containment where
  parseJSON (Object v) = do
    [parent] <- v .: "Thread"
    [child] <- v .: "Subthread"
    prob <- v .: "Effective P(needed)"
    return $ Containment parent child (prob / 100)

data Block = Block { 
    blockingThread :: RecordID
  , blockedThread :: RecordID
  , blockPercentage :: Double -- [0,1]
  }

instance FromJSON Block where
  parseJSON (Object v) = do
    [blocking] <- v .: "Blocking thread"
    [blocked] <- v .: "Blocked thread"
    percent <- v .:? "% blocked" .!= 100
    return $ Block blocking blocked (percent / 100)

data Velocity = Velocity {
    vDeveloper :: RecordID
  , vTag :: RecordID
  , vMultiplier :: Double -- [0,1]
  }

instance FromJSON Velocity where
  parseJSON (Object v) = do
    [dev] <- v .: "Developer"
    [tag] <- v .: "Task type"
    mult <- v .: "Multiplier"
    return $ Velocity dev tag mult

newtype DevName = DevName Text deriving (FromJSON, Show)

data Developer = Developer {
    devName :: DevName
  , devVelocities :: [RecordID]
  }

instance FromJSON Developer where
  parseJSON (Object v) = 
    Developer <$> v .: "Name"
              <*> v .: "Velocities"

instance Debug Developer where
  debug = show . devName

type Schedule = HashMap DevID [(Double, Maybe ThreadID)]

instance Debug (Table Developer, Schedule) where
  debug (devTbl, s) = 
       prettyRows 20 (map getRow $ Map.toList s)
    ++ "\nTOTAL RUNTIME: " ++ getRuntime s
    where
      getRow (devId, timeline) = [
        debug (select devTbl devId)
      , "working time: " ++ getWorkingTime s devId
      , "blocked time: " ++ getBlockedTime s devId 
      ]

getRuntime :: Schedule -> Double
getRuntime = max . map (last . map fst) . Map.elems

getWorkingTime :: Schedule -> DevID -> Double
getWorkingTime mp devId = 
  sum . map fst . filter (\(_,m) -> m == Nothing) . toDiffs $ mp Map.! devId

getBlockedTime :: Schedule -> DevID -> Double
getBlockedTime mp devId = 
  sum . map fst . filter (\(_,m) -> isJust m) . toDiffs $ mp Map.! devId

toDiffs :: Schedule -> Schedule 
toDiffs = Map.map (foldr diff [])
  where
    toDiff t [] = t
    toDiff (d, mode) ts = ts ++ [(d - fst (last ts), mode)]

data ScheduleParams = ScheduleParams { 
    w_unblocked :: Double
  , w_elapsed :: Double
  , w_priority :: Double
  }

instance Debug ScheduleParams where
  debug prms = prettyRows 20 [
      ["w_unblocked", show $ w_unblocked prms]
    , ["w_elapsed", show $ w_elapsed prms]
    , ["w_priority", show $ w_priority prms]
    ]

prettyRows :: Int -> [[String]] -> String  
prettyRows maxLen = unlines . map (foldl' (\s t -> s ++ " | " ++ block t) "")
  where
    block s = take maxLen s ++ replicate (max (len s) maxLen - maxLen) ' ' 

data Priority = Priority ThreadID Double

instance Debug (Table Thread, [Priority]) where 
  debug (thrTbl, priorities) = 
    prettyRows 20 $ for priorities $ \(Priority t p) -> 
      [debug (select t thrTbl), show p]

-- Debug class

class Debug a where
  debug :: a -> String

