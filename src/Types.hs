{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import           GHC.Generics

import           Control.Applicative ((<|>))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Data.Hashable
import           Data.Traversable (for)
import           Data.Foldable (foldl', foldlM)
import           Data.Maybe (isJust)

import           Debug.Trace

-- Core helpers

trace' a b = trace (a <> show b) b

lookup :: (Show k, Eq k, Hashable k, Show v) => HashMap k v -> k -> v
lookup mp k = case Map.lookup k mp of 
  Just v -> v
  Nothing -> error $ "lookup failed in map: " <> show k <> "\nsource map: " <> show mp

-- Core types

newtype RecordID = RecordID Text deriving (FromJSON, Show, Eq, Generic, Ord)
instance Hashable RecordID 

rec2str :: RecordID -> String
rec2str (RecordID rec) = T.unpack rec

class IsRecord a where
  toRec :: a -> RecordID

instance IsRecord RecordID where
  toRec = id

instance IsRecord String where
  toRec = RecordID . T.pack 

data Table a = Table {
    tableRecords :: Map.HashMap RecordID a
  , tableOffset :: Maybe Text
  } deriving (Show)

instance (FromJSON a) => FromJSON (Table a) where
  parseJSON (Object v) = do
    recs <- v .: "records" :: Parser [Value]
    parsedRecs <- foldlM parseRec Map.empty recs
    offset <- v .:? "offset"
    return $ Table parsedRecs offset
    where
      parseRec tbl (Object v) = 
            do  recId <- v .: "id"
                obj <- v .: "fields" 
                return $ Map.insert recId obj tbl
        <|> pure tbl

instance Monoid (Table a) where
  mempty = Table mempty Nothing
  mappend (Table t1 o) (Table t2 _) = Table (mappend t1 t2) o

toList :: Table a -> [(RecordID, a)]
toList = Map.toList . tableRecords

exists :: (IsRecord r) => Table a -> r -> Bool
exists tbl rec = Map.member (toRec rec) (tableRecords tbl)

select :: (IsRecord r) => Table a -> r -> a
select tbl rec = tableRecords tbl Map.! (toRec rec)

selectAll :: Table a -> [a]
selectAll = map snd . toList

selectAllKeys :: Table a -> [RecordID]
selectAllKeys = map fst . toList

selectWhere :: Table a -> (RecordID -> a -> Bool) -> [a]
selectWhere tbl f = map snd $ filter (uncurry f) (toList tbl)

selectKeyWhere :: Table a -> (RecordID -> a -> Bool) -> [RecordID]
selectKeyWhere tbl f = map fst $ filter (uncurry f) (toList tbl)

deleteWhere :: Table a -> (RecordID -> a -> Bool) -> Table a
deleteWhere (Table recs off) f = Table (Map.filterWithKey (\k v -> not $ f k v) recs) off

-- ID types

newtype ThreadID = ThreadID {getThreadId :: RecordID} deriving (Show, Eq, Ord)
instance IsRecord ThreadID where
  toRec (ThreadID rec) = rec

newtype DevID = DevID {getDevId :: RecordID} deriving (Show, Eq, Generic)
instance Hashable DevID
instance IsRecord DevID where
  toRec (DevID rec) = rec

newtype TagID = TagID {getTagId :: RecordID} deriving (Show, Eq)
instance IsRecord TagID where
  toRec (TagID rec) = rec

-- API types

data Tag = Tag {
    tagName :: Text
  , tagMultiplierIfMissing :: Double
  }

instance FromJSON Tag where
  parseJSON (Object v) = 
    Tag <$> v .: "Name" 
        <*> v .: "Multiplier if missing"

data Thread = Thread {
    threadName :: Text
  , threadTags :: [RecordID]
  , threadContainments :: [RecordID]
  , threadBlocks :: [RecordID]
  , threadStoryPts :: Double
  , threadAssignable :: Bool
  , threadFinished :: Bool
  } deriving (Show)

instance FromJSON Thread where 
  parseJSON (Object v) = 
    Thread <$> v .: "Thread name"
           <*> v .:? "Tags" .!= []
           <*> v .:? "Contained in" .!= []
           <*> v .:? "Blocks" .!= []
           <*> v .:? "Story pts" .!= 42 -- what the hell should this default be 
           <*> (boolField <$> v .: "Assignable?")
           <*> (boolField <$> v .: "Done?")
    where
      boolField :: Double -> Bool
      boolField n = n == fromInteger 1

instance Debug Thread where 
  debug = show . threadName

data Containment = Containment {
    parentThread :: RecordID
  , childThread :: RecordID
  , containmentProbability :: Double -- [0,1]
  } deriving (Show)

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
              <*> v .:? "Velocities" .!= []

instance Debug Developer where
  debug = show . devName

type Schedule = Map.HashMap DevID [(Double, Maybe ThreadID)]

instance Debug (Table Developer, Schedule) where
  debug (devTbl, s) = 
       prettyRows 20 (map getRow $ Map.toList s)
    ++ "\nTOTAL RUNTIME: " ++ show (getRuntime s)
    where
      getRow (devId, timeline) = [
          debug (select devTbl (getDevId devId))
        , "working time: " ++ show (getWorkingTime s devId)
        , "blocked time: " ++ show (getBlockedTime s devId) 
        ]

getRuntime :: Schedule -> Double
getRuntime = maximum . map (last . map fst) . Map.elems

getWorkingTime :: Schedule -> DevID -> Double
getWorkingTime mp devId = 
  sum . map fst . filter (\(_,m) -> m == Nothing) $ (toDiffs mp) Map.! devId

getBlockedTime :: Schedule -> DevID -> Double
getBlockedTime mp devId = 
  sum . map fst . filter (\(_,m) -> isJust m) $ (toDiffs mp) Map.! devId

toDiffs :: Schedule -> Schedule 
toDiffs = Map.map (foldr toDiff [])
  where
    toDiff t [] = [t]
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
    block s = take maxLen s ++ replicate (max (length s) maxLen - maxLen) ' ' 

data Priority = Priority ThreadID Double

instance Debug (Table Thread, [Priority]) where 
  debug (thrTbl, priorities) = 
    prettyRows 20 $ for priorities $ \(Priority (ThreadID t) p) -> 
      [debug (select thrTbl t), show p]

-- Debug class

class Debug a where
  debug :: a -> String

