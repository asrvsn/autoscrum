{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AirtableIO.TasksBase 
  ( 
  -- * Constants
    master_thread_name
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
  -- * Accessors
  , devTags
  , getChildren
  , getParents
  , getDescendants
  , getBlockages
  , getBlockedThreads
  , getBlockingThreads
  , getMasterThread
  ) where

import           GHC.Generics
import           Prelude hiding (lookup)
import           Data.Aeson
import           Data.Hashable (Hashable)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Airtable.Table

import Missing
import Constants

-- * Constants

master_thread_name :: ThreadName
master_thread_name = ThreadName "Valuable at work"

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

data Thread = Thread 
  { threadName :: ThreadName
  , threadTags :: [RecordID]
  , threadContainments :: [RecordID]
  , threadBlocks :: [RecordID]
  , threadStoryPts :: Double
  , threadAssignable :: Bool
  , threadFinished :: Bool
  , threadAsignee :: Maybe RecordID
  } deriving ( Show
             , Read
             )

instance FromJSON Thread where 
  parseJSON = withObject "thread" $ \v ->
    Thread <$> v .: "Thread name"
           <*> v .:? "Tags" .!= []
           <*> v .:? "Contained in" .!= []
           <*> v .:? "Blocks" .!= []
           <*> v .:? "Story pts" .!= (-1) -- ^ TODO(anand) what should this default be 
           <*> (boolField <$> v .: "Assignable?")
           <*> (boolField <$> v .: "Done?")
           <*> v .:? "Asignee"
    where
      boolField :: Double -> Bool
      boolField n = n == fromInteger 1

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
    [parent] <- v .: "Thread"
    [child] <- v .: "Subthread"
    prob <- v .: "Effective P(needed)"
    return $ Containment parent child (prob / 100)

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

getMasterThread :: Table Thread -> ThreadID
getMasterThread thrTbl = ThreadID thrId
  where
    [thrId] = vSelectKeyWhere thrTbl $ \thr -> 
                threadName thr == master_thread_name
