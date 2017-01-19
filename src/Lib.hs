{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Types

import System.Random

import Data.Monoid
import qualified Data.HashMap.Strict as Map

import Bayes.BayesianNetwork

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

-- Helpers

devTags :: Table Velocity
        -> Developer
        -> [TagID] 
devTags velTbl = 
  map (TagID . vTag . select velTbl) . devVelocities 

getChildren :: Table Containment
            -> ThreadID 
            -> [ThreadID]
getChildren cntTbl thrId = 
  map childThread $ 
    selectWhere cntTbl $ \_ cnt -> 
      parentThread cnt == thrId

getParents :: Table Containment 
           -> ThreadID  
           -> [ThreadID]
getParents cntTbl thrId = 
  map parentThread $ 
    selectWhere cntTbl $ \_ cnt -> 
      childThread cnt == thrId

getBlockedThreads :: Table Block 
                  -> ThreadID
                  -> [(ThreadID, Double)]
getBlockedThreads blkTbl thrId = 
  map (\blk -> (blockedThread blk, blockPercentage blk)) $ 
    selectWhere blkTbl $ \_ blk -> 
      blockingThread blk == thrId

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
               -> Table Containment
               -> ThreadID
               -> Double
blockageFactor thrTbl blkTbl cntTbl (ThreadID thrId) = 
  product $ map (\b -> 1 / (1 - b)) blockages
  where
    blockages = map getBlockage (getBlockedThreads blkTbl thrId)
    getBlockage (blkThrId, blkPerc) = 
      let blkThr   = select thrTbl blkThrId 
      in  if threadAssignable blkThr
            then blkPerc
            else case getChildren cntTbl blkThrId of
              [] -> blkPerc
              xs -> if all threadFinished (map (select thrTbl) xs) 
                then 0
                else blkPerc

taskSample :: Table Thread
           -> Table Containment
           -> ThreadID 
           -> IO [ThreadID]
taskSample thrTbl cntTbl thrId = 
  sample $ 
    inferMarginalInclusions thrTbl cntTbl thrId

containmentPdf :: Table Thread 
               -> Table Containment 
               -> ThreadID 
               -> [(Double, ThreadID)]
containmentPdf thrTbl cntTbl thrId = 
  map (\c -> (containmentProbability c, childThread c)) $ 
    selectWhere cntTbl $ \_ cnt -> 
      parentThread cnt == thrId

sample :: [(Double, a)] -> IO [a]
sample pdf = catMaybes <$> mapM event pdf
  where
    event (p, a) = do
      outcome <- unfairCoin p
      return $ 
        if outcome then Just a else Nothing 

unfairCoin :: Double -> IO Bool
unfairCoin p = flip <$> getStdRandom (randomR (1,100))
  where
    flip r = r < (p * 100) 

inferMarginalInclusion :: Table Thread
                       -> Table Containment
                       -> ThreadID -- Parent task
                       -> ThreadID -- Descendant task
                       -> Double   -- P(descendant task | parent task=T)
inferMarginalInclusion thrTbl cntTbl parentId childId = 
  case posterior junctionTree [varMap Map.! childId] of 
    Just p  -> p
    Nothing -> error "could not find cluster for given thread ID"

  where
    junctionTree = 
      updateEvidence [(varMap Map.! parentId) =: True] $ 
        createJunctionTree nodeComparisonForTriangulation net

    (varMap, net) = runBN $ do
      varMap <- createVars 
      mapM_ (addFactor varMap . snd) (toList cntTbl)
      return varMap

    createVars = do
      let thrIds = map fst (toList thrTbl)
      let mkVar mp tid = do var <- variable tid (undefined :: Bool)
                            return $ Map.insert tid var mp
      foldlM mkVar Map.empty thrIds

    addFactor varMap cnt = do
      let parent = varMap Map.! (parentThread cnt) 
      let child = varMap Map.! (childThread cnt)
      let contP = containmentProbability cnt
      cpt child [parent] ~~ [
          1           -- p(child=F | parent=F)
        , 1 - contP   -- p(child=F | parent=T)
        , 0           -- p(child=T | parent=F)
        , contP       -- p(child=T | parent=T)
        ]

taskPriority :: Table Thread
             -> Table Block
             -> Table Containment
             -> ThreadID -- master thread ID
             -> ThreadID -- given thread ID
             -> Double
taskPriority thrTbl blkTbl cntTbl masterThrId thrId = 
  case getBlockedThreads blkTbl thrId of 
    [] -> 
      let p = inferMarginalInclusion thrTbl cntTbl masterThrId thrId
          thr = select thrTbl thrId
          story = threadStoryPts thr
          blockage = blockageFactor thrTbl blkTbl cntTbl thrId 
      in (-1) * log (1 - (15/16) * p) / (story * blockage)
    xs -> sum $ for xs $ \(blkThrId, blkPerc) -> 
      taskPriority thrTbl blkTbl cntTbl blkThrId * blkPerc

schedule :: Table Thread
         -> Table Block
         -> Table Containment
         -> Table Developer 
         -> Table Tag 
         -> Table Velocity 
         -> [(ThreadID, DevID)]
schedule thrTbl blkTbl cntTbl devTbl tagTbl velTbl = undefined
  where
    tasks = selectWhere thrTbl $ \_ thr -> threadAssignable thr