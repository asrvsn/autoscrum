{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module AirtableIO.TasksBase 
  ( 
  -- * Constants
    master_thread_name
  -- * Base type
  , TasksBase(..)
  -- * ID types
  , ThreadID(..)
  , DevID(..)
  , TagID(..)
  -- * Tags
  , Tag(..)
  -- * Threads
  , ThreadName(..)
  , Thread(..)
  -- * Containments
  , Containment(..)
  -- * Blocks
  , Block(..)
  -- * Velocities
  , Velocity(..)
  -- * Developers
  , DevName(..)
  , Developer(..)
  -- * Retrieval
  , getTasksBase
  -- * Accessors
  , devTags
  , getChildren
  , getParents
  , getDescendants
  , getBlockages
  , getBlockedThreads
  , getBlockingThreads
  , findThr
  ) where

import           GHC.Generics
import           Prelude hiding (lookup)
import           Data.Aeson
import           Data.Hashable (Hashable)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Monoid
import           Control.Concurrent
import           Airtable.Table
import           Airtable.Query

import Missing
import Constants

-- * Constants

master_thread_name :: ThreadName
master_thread_name = ThreadName "Valuable at work"

-- * Base type

data TasksBase = TasksBase
  { threads :: Table Thread
  , blocks :: Table Block
  , developers :: Table Developer
  , containments :: Table Containment
  , tags :: Table Tag
  , velocities :: Table Velocity
  }

-- * ID types

newtype ThreadID = ThreadID 
  { getThreadId :: RecordID 
  } deriving ( Show
             , Read
             , Eq
             , Ord
             , Generic
             )
instance Hashable ThreadID
instance HasRecordId ThreadID where
  toRecId (ThreadID rec) = rec

newtype DevID = DevID 
  { getDevId :: RecordID 
  } deriving ( Show
             , Read
             , Eq
             , Generic
             )
instance Hashable DevID
instance HasRecordId DevID where
  toRecId (DevID rec) = rec

newtype TagID = TagID 
  { getTagId :: RecordID
  } deriving ( Show
             , Read
             , Eq
             )
instance HasRecordId TagID where
  toRecId (TagID rec) = rec

-- * Tags

data Tag = Tag 
  { tagName :: Text
  , tagMultiplierIfMissing :: Double
  } deriving ( Show
             , Read
             )

instance FromJSON Tag where
  parseJSON = withObject "tag" $ \v ->  
    Tag <$> v .: "Name" 
        <*> v .: "Multiplier if missing"

-- * Threads

newtype ThreadName = 
  ThreadName Text deriving ( FromJSON
                           , ToJSON
                           , Show
                           , Read
                           , Eq
                           )

data ThreadStatus 
  = WorkingOn
  | Diffed
  | Done
  | Blocked
  | OnPause

instance FromJSON ThreadStatus where
  parseJSON = withObject "thread status" $ \case
    "Working On" -> return WorkingOn
    "Diffed" -> return Diffed
    "Done" -> return Done
    "Blocked" -> return Blocked
    "On pause" -> return OnPause

instance ToJSON ThreadStatus where
  toJSON = \case
    WorkingOn -> String "Working on"
    Diffed -> String "Diffed"
    Done -> String "Done"
    Blocked -> String "Blocked"
    OnPause -> String "On pause"

data Thread = Thread 
  { threadName :: ThreadName
  , threadTags :: [RecordID]
  , threadContainments :: [RecordID]
  , threadBlocks :: [RecordID]
  , threadStoryPts :: Double
  , threadAssignable :: Bool
  , threadFinished :: Bool
  , threadAssignee :: Maybe RecordID
  , threadStatus :: Maybe ThreadStatus
  } deriving ( Show
             , Read
             , Eq
             )

instance FromJSON Thread where 
  parseJSON = withObject "thread" $ \v ->
    Thread <$> v .:? "Thread name" .!= ThreadName ""
           <*> v .:? "Tags" .!= []
           <*> v .:? "Contained in" .!= []
           <*> v .:? "Blocks" .!= []
           <*> v .:? "Story pts" .!= (-1) -- ^ TODO(anand) what should this default be 
           <*> (boolField <$> v .: "Assignable?")
           <*> (boolField <$> v .: "Done?")
           <*> (headMay <$> v .:? "Assignee" .!= [])
           <*> v .:? "Status"
    where
      boolField :: Double -> Bool
      boolField n = n == fromInteger 1
      headMay :: [a] -> Maybe a
      headMay []     = Nothing
      headMay (x:xs) = Just x

instance ToJSON Thread where
  toJSON thr = object [ "Thread name" .= threadName thr
                      , "Tags" .= threadTags thr
                      , "Contained in" .= threadContainments thr
                      , "Blocks" .= threadBlocks thr
                      , "Story pts" .= threadStoryPts thr
                      , "Assignable?" .= boolField (threadAssignable thr)
                      , "Done?" .= boolField (threadFinished thr)
                      , "Assignee" .= maybe [] (\a -> [a]) (threadAssignee thr)
                      , "Status" .= threadStatus thr
                      ]
    where
      boolField :: Bool -> Double
      boolField b = if b then 1 else 0

instance Debug Thread where 
  debug = show . threadName

-- * Containments

data Containment = Containment 
  { parentThread :: RecordID
  , childThread :: RecordID
  , containmentProbability :: Double -- ^ in the range [0,1]
  } deriving ( Show
             , Read
             )

instance FromJSON Containment where
  parseJSON = withObject "containment" $ \v -> do
    parent <- listField "containment parent error" v <$> v .: "Thread"
    child <- listField "containment child error" v <$> v .: "Subthread"
    prob <- v .: "Effective P(needed)"
    return $ Containment parent child (prob / 100)
    where
      listField err v xs = case xs of 
        [x] -> x
        _ -> error $ err <> ": " <> show v

-- * Blocks

data Block = Block 
  { blockingThread :: RecordID
  , blockedThread :: RecordID
  , blockPercentage :: Double -- ^ in the range [0,1]
  } deriving ( Show
             , Read
             )

instance FromJSON Block where
  parseJSON = withObject "block" $ \v -> do
    [blocking] <- v .: "Blocking thread"
    [blocked] <- v .: "Blocked thread"
    percent <- v .:? "% blocked" .!= 100
    return $ Block blocking blocked (percent / 100)

-- * Velocities

data Velocity = Velocity 
  { vDeveloper :: RecordID
  , vTag :: RecordID
  , vMultiplier :: Double -- in the range [0,1]
  } deriving ( Show
             , Read
             )

instance FromJSON Velocity where
  parseJSON = withObject "Velocity" $ \v -> do
    [dev] <- v .: "Developer"
    [tag] <- v .: "Task type"
    mult <- v .: "Multiplier"
    return $ Velocity dev tag mult

-- * Developers

newtype DevName = 
  DevName Text deriving ( FromJSON
                        , ToJSON
                        , Show
                        , Read
                        , Eq
                        , Hashable
                        )

data Developer = Developer 
  { devName :: DevName
  , devVelocities :: [RecordID]
  } deriving ( Show
             , Read
             )

instance FromJSON Developer where
  parseJSON = withObject "developer" $ \v -> 
    Developer <$> v .: "Name"
              <*> v .:? "Velocities" .!= []

instance Debug Developer where
  debug = show . devName

-- * Retrieval

getTasksBase :: AirtableOptions -> IO TasksBase
getTasksBase opts = wait =<< 
  (TasksBase
    <$> async (getRecords opts "Threads")
    <*> async (getRecords opts "Blocks")
    <*> async (getRecords opts "Developers")
    <*> async (getRecords opts "Containments")
    <*> async (getRecords opts "Task types")
    <*> async (getRecords opts "Velocities")
  )

-- * Accessors

devTags :: Table Velocity
        -> Developer
        -> [TagID] 
devTags velTbl = 
  map (TagID . vTag . vSelect velTbl) . devVelocities 

getChildren :: Table Containment
            -> ThreadID 
            -> [ThreadID]
getChildren cntTbl thrId = 
  map (ThreadID . childThread) $ 
    vSelectWhere cntTbl $ \cnt -> 
      parentThread cnt == getThreadId thrId

getParents :: Table Containment 
           -> ThreadID  
           -> [ThreadID]
getParents cntTbl thrId = 
  map (ThreadID . parentThread) $ 
    vSelectWhere cntTbl $ \cnt -> 
      childThread cnt == getThreadId thrId

getDescendants :: Table Containment
               -> ThreadID 
               -> [ThreadID]
getDescendants cntTbl thrId = 
  Set.toList $ rec Set.empty (getChildren cntTbl thrId) 
  where
    rec a [] = a
    rec a cs = rec (foldr Set.insert a cs) (foldMap (getChildren cntTbl) cs)

getBlockages :: Table Block 
              -> Table Thread
             -> ThreadID 
             -> [(ThreadID, Double)]
getBlockages blkTbl thrTbl thrId = 
  map (\b -> (ThreadID $ blockedThread b, blockPercentage b)) $ 
    vSelectWhere blkTbl $ \blk -> 
         blockingThread blk == getThreadId thrId
      && not (threadFinished $ vSelect thrTbl (blockedThread blk))

getBlockedThreads :: Table Block 
                  -> Table Thread
                  -> ThreadID
                  -> [ThreadID]
getBlockedThreads blkTbl thrTbl thrId = 
  map (ThreadID . blockedThread) $ 
    vSelectWhere blkTbl $ \blk -> 
         blockingThread blk == getThreadId thrId
      && not (threadFinished $ vSelect thrTbl (blockedThread blk))

getBlockingThreads :: Table Block 
                    -> Table Thread
                    -> ThreadID
                    -> [ThreadID]
getBlockingThreads blkTbl thrTbl thrId = 
  map (ThreadID . blockingThread) $ 
    vSelectWhere blkTbl $ \blk -> 
         blockedThread blk == getThreadId thrId
      && not (threadFinished $ vSelect thrTbl thrId)

findThr :: Table Thread -> ThreadID
findThr thrTbl thrName = case result of 
  [thrId] -> Just $ ThreadID thrId
  _ -> Nothing
  where
    result = vSelectKeyWhere thrTbl $ \thr -> 
                threadName thr == thrName

