module AirtableComputation.BayesNet 
  ( 
  -- * Types
    BayesNet(..)
  -- * Bayes net querying
  , marginal
  , sampleTable
  ) where

import           GHC.Stack
import           Bayes.Factor 
import           Bayes.Factor.CPT (CPT)
import           Bayes.FactorElimination ( posterior
                                         , JunctionTree
                                         )
import           Airtable.Table
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Hashable (Hashable)
import           System.Random (randomR, getStdRandom)

-- * Types

data BayesNet a = BayesNet 
  { netVars :: HashMap a (TDV Bool)
  , juncTree :: JunctionTree CPT
  }

-- * Bayes net querying

marginal :: (Hashable a, Eq a, Show a, HasCallStack) => BayesNet a -> a -> Double
marginal bn a = 
  case Map.lookup a (netVars bn) of 
    Just v -> factorNorm $ 
      case posterior (juncTree bn) [v] of 
        Just p  -> p
        Nothing -> error $ "BayesNet: could not find cluster in for given accessor: " <> show a
    Nothing -> error $ "BayesNet: could not find variable for given accessor:" <> show a

sampleTable :: BayesNet RecordID -> Table a -> IO (Table a)
sampleTable bn tbl = do
  let recIds_ = selectAllKeys tbl 
  let recPs   = map (marginal bn) recIds_
  recIds <- sample (zip recPs recIds_)
  return $ 
    deleteWhere tbl $ \rec -> 
      recordId rec `notElem` recIds
  where

    sample :: [(Double, a)] -> IO [a]
    sample pdf = catMaybes <$> mapM event pdf
      where
        event (p, a) = do
          outcome <- unfairCoin p
          return $ 
            if outcome then Just a else Nothing 
            
    unfairCoin :: Double -> IO Bool
    unfairCoin p = flipCoin <$> getStdRandom (randomR (1,100))
      where
        flipCoin r = r <= (p * 100) 
