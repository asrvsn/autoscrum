{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           Missing

import           Prelude hiding (lookup)

import           GHC.Generics
import           GHC.Stack

import           Control.Applicative ((<|>))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List (maximum, minimum)
import           Data.Monoid
import           Data.Hashable
import           Data.Traversable (for)
import           Data.Foldable (foldl', foldlM)
import           Data.Maybe (isJust, catMaybes)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Time.Clock (UTCTime)

import           Bayes.Factor.CPT (CPT)
import           Bayes.Factor (TDV)
import           Bayes.FactorElimination (JunctionTree)


import           Airtable.Table




