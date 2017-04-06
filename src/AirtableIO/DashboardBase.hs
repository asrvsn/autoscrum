{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AirtableIO.DashboardBase 
  ( 
  -- * Created/Updated/Deleted types
    CUDHistory(..)
  , CUD(..)
  , tableCUDHistory
  -- * Time estimations
  , TimeEstimation(..)
  -- * Plot links
  , PlotLink(..)
  , PlotType(..)
  ) where

import           Data.Aeson 
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (catMaybes)
import           Airtable.Table

-- * Created/Updated/Deleted types

newtype CUDHistory a = CUDHistory [CUD a]

data CUD a 
  = Created a
  | Updated { old :: a, new :: a }
  | Deleted a

tableCUDHistory :: (a -> a -> Bool) -> Table a -> Table a -> CUDHistory a
tableCUDHistory cmp tbl_ tbl = 
  CUDHistory $ 
       map Created created
    ++ map (uncurry Updated) updated
    ++ map Deleted deleted
  where
    created = 
      map recordObj . Map.elems $ 
        Map.difference (tableRecords tbl) (tableRecords tbl_) 
    updCmb rec rec' = 
      let obj  = recordObj rec
          obj' = recordObj rec'
      in if cmp obj obj' then Nothing else Just (obj, obj')
    updated = 
      catMaybes $ 
        Map.elems $ 
          Map.intersectionWith updCmb (tableRecords tbl_) (tableRecords tbl)
    deleted = 
      map recordObj . Map.elems $ 
        Map.difference (tableRecords tbl_) (tableRecords tbl)

-- * Time estimations

data TimeEstimation = TimeEstimation
  { runDate :: UTCTime
  , estParentThread :: Text
  , est20 :: Double
  , est50 :: Double
  , est80 :: Double
  }

instance FromJSON TimeEstimation where
  parseJSON = withObject "time estimation" $ \v ->
    TimeEstimation <$> v .: "Run date"
                   <*> v .: "Parent thread"
                   <*> v .: "20% estimate"
                   <*> v .: "50% estimate"
                   <*> v .: "80% estimate" 

instance ToJSON TimeEstimation where
  toJSON tEst = 
    object [ "Run date" .= runDate tEst
           , "Parent thread" .= estParentThread tEst
           , "20% estimate" .= est20 tEst
           , "50% estimate" .= est50 tEst
           , "80% estimate" .= est80 tEst
           ] 

-- * Plot links

data PlotType 
  = MedGanttChart
  | EstOverTime

instance FromJSON PlotType where
  parseJSON = withText "plot type" $ \case
    "Median gantt chart"  -> pure MedGanttChart
    "Estimates over time" -> pure EstOverTime
    _ -> fail "plot type not found"

instance ToJSON PlotType where
  toJSON = \case 
    MedGanttChart -> String "Median gantt chart"
    EstOverTime   -> String "Estimates over time"

data PlotLink = PlotLink
  { plotThread :: Text
  , plotLink :: String
  , plotType :: PlotType 
  , plotLastUpdated :: UTCTime
  }

instance FromJSON PlotLink where
  parseJSON = withObject "plot link" $ \v -> do
    [linkType] <- v .: "Link type"
    PlotLink <$> v .: "Parent thread"
             <*> v .: "Link"
             <*> pure linkType
             <*> v .: "Last updated"

instance ToJSON PlotLink where
  toJSON plt = 
    object [ "Parent thread" .= plotThread plt
           , "Link" .= plotLink plt
           , "Link type" .= [plotType plt]
           , "Last updated" .= plotLastUpdated plt
           ]