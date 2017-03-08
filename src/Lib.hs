{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import           Types
import           Missing
import           Constants

import           Prelude hiding (lookup)

import           GHC.Stack
import           System.Random
import           System.Directory (doesFileExist)
import           System.Process (system)
import           Network.Wreq
import           Airtable.Table
import           Airtable.Query
import           Control.Lens ((^.), (.~), (&))
import           Text.Read (readMaybe)
import           Data.Monoid
import           Data.Hashable
import           Data.List (find, minimumBy)
import           Data.Aeson (FromJSON, ToJSON, eitherDecode)
import           Data.Maybe (catMaybes)
import           Data.Foldable (foldlM)
import           Data.Function (on)
import           Data.Traversable (for)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import           Control.Monad.State

-- bayes net stuff

-- Computation



