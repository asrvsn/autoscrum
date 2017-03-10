{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Missing 
  ( mean
  , stddev
  , var
  , l2_norm
  , standardize
  , minimumOr
  , maximumOr
  , trace'
  , lookup
  , Debug(..)
  , prettyRows
  ) where

import           Prelude hiding (lookup)
import           GHC.Stack
import           Debug.Trace
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Hashable
import           Data.Monoid
import           Data.List (minimum, maximum)
import           Data.Foldable (foldl')

-- These 3 functions are copied from https://hackage.haskell.org/package/hstats-0.3/docs/src/Math-Statistics.html#mean
-- because stack cannot solve the package's dependencies.

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- |Standard deviation of sample
stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

-- |Sample variance
var :: (Floating a) => [a] -> a
var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)

-----

l2_norm :: Floating a => [a] -> a
l2_norm xs = sqrt $ sum (map (\x -> x ** 2) xs)

standardize :: Floating a => [a] -> [a]
standardize xs = map (\x -> (x - mu) / sigma) xs
  where
    mu = mean xs
    sigma = stddev xs

minimumOr :: (Ord a) => a -> [a] -> a
minimumOr def [] = def
minimumOr _ xs = minimum xs

maximumOr :: (Ord a) => a -> [a] -> a
maximumOr def [] = def
maximumOr _ xs = maximum xs

trace' a b = trace (a <> show b) b

lookup :: (HasCallStack, Show k, Eq k, Hashable k, Show v) => HashMap k v -> k -> v
lookup mp k = case Map.lookup k mp of 
  Just v -> v
  Nothing -> error $ "lookup failed in map: " <> show k

-- Debug class

class Debug a where
  debug :: a -> String

prettyRows :: Int -> [[String]] -> String  
prettyRows maxLen = unlines . map (concat . map (\s -> " | " ++ block s))
  where
    block s = take maxLen s ++ replicate (max (length s) maxLen - length s) ' ' 
