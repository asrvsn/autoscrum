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
import           System.Exit (ExitCode(..))
import           Network.Wreq
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
import           Bayes (SBN)
import           Bayes.Factor 
import           Bayes.Factor.CPT (CPT)
import           Bayes.BayesianNetwork
import           Bayes.FactorElimination ( nodeComparisonForTriangulation
                                         , createJunctionTree
                                         , changeEvidence
                                         , posterior
                                         , JunctionTree
                                         )


-- General methods

yn :: String -> IO a -> IO a -> IO a
yn ask y n = do
  putStrLn $ "\n" ++ ask ++ " (Y/N)"
  resp <- getLine
  case resp of 
    "y" -> y
    "Y" -> y
    "n" -> n
    "N" -> n
    _   -> yn ask y n

ynCached :: (Show a, Read a) => String -> IO a -> IO a
ynCached fname n = do
  b <- doesFileExist (fname <> ".cache")
  if b 
    then yn ("Use cached " <> fname <> "?") (retrieve fname) n
    else do
      a <- n
      persist fname a
      return a

cmdOptions :: [String] -> (String -> IO ()) -> IO ()
cmdOptions opts f = do
  putStrLn "\nThe following options are available:"
  putStrLn $ "\n" <> unlines prettyOpts
  putStrLn "\nWhat would you like to do?"
  resp <- getLine
  case find (== resp) optsWithExit of 
    Just _ -> cont resp
    Nothing -> 
      case readMaybe resp :: Maybe Int of 
        Just i -> if i <= length optsWithExit
          then cont (optsWithExit !! (i - 1))
          else err
        Nothing -> err
  where
    optsWithExit = opts <> ["exit"]
    prettyOpts = zipWith (\n o -> show n <> ") " <> o) [1..] optsWithExit
    err = do putStrLn "That's not an available option."
             cmdOptions opts f
    cont resp = case resp of 
                  "exit" -> return ()
                  _ -> f resp >> cmdOptions opts f

enterParameters :: IO ScheduleParams 
enterParameters = 
  ScheduleParams <$> reqDouble "w_completed"
                 <*> reqDouble "w_priority"
  where
    reqDouble s = do
      putStrLn $ "Enter " <> s
      resp <- getLine
      case readMaybe resp :: Maybe Double of 
        Just d -> return d
        Nothing -> reqDouble s

mightNeedSudo :: String -> IO ExitCode
mightNeedSudo cmd = do
  e <- system cmd
  case e of 
    ExitFailure 1 -> do
      putStrLn $ show cmd <> " failed, trying with sudo"
      system $ "sudo " <> cmd
    _ -> return e

-- API methods

persist :: (Show a) => String -> a -> IO ()
persist fname a = writeFile (fname <> ".cache") (show a)

retrieve :: (Read a) => String -> IO a
retrieve fname = read <$> readFile (fname <> ".cache")

visualizeSchedule :: IO () 
visualizeSchedule = do
  system "python visualize.py"
  url <- readFile "schedule_vis.url"
  system $ "google-chrome " <> url
  return ()

getTable :: (FromJSON a, Show a, Read a) => String -> IO (Table a)
getTable tblStr = ynCached (tblStr <> "_table") $ do
  tbl <- getTableParts opts url 
  putStrLn $ "Downloaded " <> tblStr
  return tbl
  where
    opts = defaults & header "Authorization" .~ ["Bearer " <> api_key] 
    url  = api_url <> tblStr

getTableParts :: (FromJSON a) => Options -> String -> IO (Table a)
getTableParts opts url = do
  resp <- getWith opts url
  getMore (fromResp resp)
  where
    getMore tbl = case tableOffset tbl of 
      Just offset -> do
        resp <- getWith (opts & param "offset" .~ [offset]) url
        putStrLn $ "Part " <> show offset
        getMore $ fromResp resp <> tbl
      Nothing -> 
        pure tbl

fromResp :: (FromJSON a) => Response ByteString -> Table a
fromResp r = decoder $ r ^. responseBody
  where
    decoder b = case eitherDecode b of 
      Left e -> error $ e <> "\nSource string: " <> show b
      Right r -> r

uploadPrioritization :: Prioritization -> IO ()
uploadPrioritization = undefined

uploadSchedule :: Schedule -> IO ()
uploadSchedule = undefined

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
  map (ThreadID . childThread) $ 
    selectWhere cntTbl $ \_ cnt -> 
      parentThread cnt == getThreadId thrId

getParents :: Table Containment 
           -> ThreadID  
           -> [ThreadID]
getParents cntTbl thrId = 
  map (ThreadID . parentThread) $ 
    selectWhere cntTbl $ \_ cnt -> 
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
    selectWhere blkTbl $ \_ blk -> 
         blockingThread blk == getThreadId thrId
      && not (threadFinished $ select thrTbl (blockedThread blk))

getBlockedThreads :: Table Block 
                  -> Table Thread
                  -> ThreadID
                  -> [ThreadID]
getBlockedThreads blkTbl thrTbl thrId = 
  map (ThreadID . blockedThread) $ 
    selectWhere blkTbl $ \_ blk -> 
         blockingThread blk == getThreadId thrId
      && not (threadFinished $ select thrTbl (blockedThread blk))

getBlockingThreads :: Table Block 
                    -> Table Thread
                    -> ThreadID
                    -> [ThreadID]
getBlockingThreads blkTbl thrTbl thrId = 
  map (ThreadID . blockingThread) $ 
    selectWhere blkTbl $ \_ blk -> 
         blockedThread blk == getThreadId thrId
      && not (threadFinished $ select thrTbl thrId)

getMasterThread :: Table Thread -> ThreadID
getMasterThread thrTbl = ThreadID thrId
  where
    [thrId] = selectKeyWhere thrTbl $ \_ thr -> 
                threadName thr == master_thread_name

-- Data cleaning

reconcileTables :: Table Block
                  -> Table Containment 
                  -> Table Thread 
                  -> (Table Block, Table Containment, Table Thread)
reconcileTables blkTbl_ cntTbl_ thrTbl_ =
  (blkTbl, cntTbl, thrTbl)
  where
    blkTbl =  deleteWhere blkTbl_ $ \_ blk -> 
                any (not . exists thrTbl_) [blockingThread blk, blockedThread blk]
    cntTbl =  deleteWhere cntTbl_ $ \_ cnt -> 
                any (not . exists thrTbl_) [parentThread cnt, childThread cnt] 
    thrTbl =  deleteWhere thrTbl_ $ \_ thr -> 
                any (not . exists cntTbl_) (threadContainments thr ++ threadBlocks thr)


-- Computation

taskCompletionTime :: Table Tag
                   -> Table Thread
                   -> Table Developer
                   -> Table Velocity
                   -> ThreadID 
                   -> DevID 
                   -> Double
taskCompletionTime tagTbl thrTbl devTbl velTbl (ThreadID thrId) (DevID devId) = 
  storyPts / (product devMultipliers * product missingMultipliers)
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
blockageFactor thrTbl blkTbl cntTbl thrId = 
  product $ map (\b -> 1 / (1 - b)) blockages
  where
    blockages = map compute (getBlockages blkTbl thrTbl thrId)
    compute (blkThrId, blkPerc) = 
      let blkThr   = select thrTbl blkThrId 
      in  if threadAssignable blkThr
            then blkPerc
            else case getChildren cntTbl blkThrId of
              [] -> blkPerc
              xs -> if all threadFinished (map (select thrTbl) xs) 
                then 0
                else blkPerc

-- taskSample :: Table Thread
--            -> Table Containment
--            -> ThreadID -- parent thread
--            -> IO [ThreadID]
-- taskSample thrTbl cntTbl parentThrId = 
--   sample $ 
--     map 
--       (\childThrId -> 
--         (marginalInclusion thrTbl cntTbl childThrId, childThrId))
--       (getDescendants cntTbl parentThrId)

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

bayesNet :: Table Thread 
         -> Table Containment 
         -> (HashMap RecordID (TDV Bool), JunctionTree CPT)
bayesNet thrTbl cntTbl = 
  let varMapKeys = Set.fromList (Map.keys varMap)
  in if varMapKeys == varKeys
    then (varMap, junctionTree)
    else error $ 
         "invariant violation: the variable map has additional records \n" 
      <> show (varMapKeys `Set.difference` varKeys)
      <> "\n and the threads table has additional records \n"
      <> show (varKeys `Set.difference` varMapKeys)
  where
    junctionTree = createJunctionTree nodeComparisonForTriangulation net

    (varMap, net) = construction

    construction :: (HashMap RecordID (TDV Bool), SBN CPT)
    construction = runBN $ do
      varMap <- foldlM createVar Map.empty varKeys
      mapM_ (setContainment varMap) (selectAll cntTbl)
      -- initialize all root threads
      mapM_ (setCertainContainment varMap) rootKeys
      -- initialize all dis connected threads (this is for inconsistent data)
      mapM_ (setCertainContainment varMap) (varKeys `Set.difference` cntKeys)
      return varMap

    createVar mp thrId = do
      var <- variable (rec2str thrId) (undefined :: Bool)
      return $ Map.insert thrId var mp

    setContainment varMap cnt = do
      let parent = varMap `lookup` (parentThread cnt) 
      let child = varMap `lookup` (childThread cnt)
      let contP = containmentProbability cnt
      cpt child [parent] ~~ [
          1           -- p(child=F | parent=F)
        , 1 - contP   -- p(child=F | parent=T)
        , 0           -- p(child=T | parent=F)
        , contP       -- p(child=T | parent=T)
        ]

    setCertainContainment varMap thrId = 
      proba (varMap `lookup` thrId) ~~ [0, 1]

    varKeys = Set.fromList (selectAllKeys thrTbl)
    rootKeys = Set.filter (null . getParents cntTbl . ThreadID) varKeys
    cntKeys = 
        Set.fromList 
      . foldMap (\cnt -> [parentThread cnt, childThread cnt])
      $ selectAll cntTbl 


    masterThrId = getMasterThread thrTbl

marginalInclusion :: (HasCallStack) 
                  => HashMap RecordID (TDV Bool) 
                  -> JunctionTree CPT
                  -> ThreadID
                  -> Double
marginalInclusion varMap juncTree childId = 
  factorNorm $ 
    case posterior juncTree [varMap `lookup` (getThreadId childId)] of 
      Just p  -> p
      Nothing -> error $ "could not find cluster for given thread ID: " <> show childId

taskPriority :: Table Thread
             -> Table Block
             -> Table Containment
             -> HashMap RecordID (TDV Bool)
             -> JunctionTree CPT
             -> ThreadID -- given thread ID
             -> Double
taskPriority thrTbl blkTbl cntTbl varMap juncTree thrId = 
  case getBlockages blkTbl thrTbl thrId of 

    [] -> 
      let p = marginalInclusion varMap juncTree thrId
          thr = select thrTbl thrId
          story = threadStoryPts thr
          blockage = blockageFactor thrTbl blkTbl cntTbl thrId 

      in (-1) * log (1 - (63/64) * p) / (story * blockage)

    xs -> 
      let blockagePriority blkThrId blkPerc = 
            taskPriority thrTbl blkTbl cntTbl varMap juncTree blkThrId * blkPerc

      in sum $ map (uncurry blockagePriority) xs

computePrioritization :: Table Thread
                   -> Table Block
                   -> Table Containment
                   -> Prioritization
computePrioritization thrTbl blkTbl cntTbl = 
  Prioritization . Map.fromList $ 
    zip tasks (map getPriority tasks)
  where
    tasks       = map ThreadID (selectAllKeys thrTbl)
    (varMap, juncTree) = bayesNet thrTbl cntTbl
    getPriority = taskPriority thrTbl blkTbl cntTbl varMap juncTree 

computeSchedule :: Table Thread
                 -> Table Block
                 -> Table Containment
                 -> Table Developer 
                 -> Table Tag 
                 -> Table Velocity 
                 -> ScheduleParams
                 -> Schedule
computeSchedule thrTbl blkTbl cntTbl devTbl tagTbl velTbl prms = 
  execState flushTasks initSchedule
  where
    devs  = selectAllKeys devTbl

    initSchedule = Map.fromList $ zip (map DevID devs) (repeat [])

    masterThrId = getMasterThread thrTbl

    prioritization = computePrioritization thrTbl blkTbl cntTbl

    (varMap, juncTree) = bayesNet thrTbl cntTbl

    flushTasks :: State Schedule ()
    flushTasks = do
      ts <- getUnblockedTasks
      case ts of 
        [] -> return ()
        _  -> do
          let possibleAssignments = [(t, DevID d) | t <- ts, d <- devs]
          features <- mapM (uncurry assignmentFeatures) possibleAssignments
          let losses = map (loss prms) (rescaleFeatures features)
          let ((t, d), _) = minimumBy (compare `on` snd) (zip possibleAssignments losses)
          assign t d
          flushTasks

    getUnblockedTasks :: State Schedule [ThreadID]
    getUnblockedTasks = do
      completed <- gets completedTasks
      return $ map ThreadID $ 
        selectKeyWhere thrTbl $ \thrId thr -> 
          let blocks = Set.fromList (getBlockingThreads blkTbl thrTbl $ ThreadID thrId) 
          in    blocks `Set.isSubsetOf` completed
             && ThreadID thrId `Set.notMember` completed
             && not (threadFinished thr)

    assignmentFeatures :: ThreadID -> DevID -> State Schedule Features
    assignmentFeatures thrId devId = do
      -- (1) time to task unblocked (by dev availability as well as blocking threads)
      t_task <- getTaskAvailability thrId
      t_dev  <- getDevAvailability devId
      let t_unblocked = max t_task t_dev
      -- (2) dev completion time
      let t_elapsed = taskCompletionTime tagTbl thrTbl devTbl velTbl thrId devId   
      -- (3) task priority
      let priority = getPrioritization prioritization `lookup` thrId
      return $ Features {
          f_completed = t_unblocked + t_elapsed
        , f_priority = priority
        }

    assign :: ThreadID -> DevID -> State Schedule ()
    assign thrId devId = do
      let dt_task = taskCompletionTime tagTbl thrTbl devTbl velTbl thrId devId
      t_task <- getTaskAvailability thrId
      t_dev  <- getDevAvailability devId
      let elapsed = if t_task > t_dev 
                      then [(t_task + dt_task, Just thrId), (t_task, Nothing)]
                      else [(t_dev + dt_task, Just thrId)]
      modify $ 
        Map.adjust (\ts -> elapsed ++ ts) devId

    getTaskAvailability :: HasCallStack => ThreadID -> State Schedule Double
    getTaskAvailability thrId = do
      let blkThrs = getBlockingThreads blkTbl thrTbl thrId
      blkTimes <- mapM getCompletedTaskTime blkThrs
      return $ case blkTimes of 
        [] -> 0
        _  -> assertPositive $ maximum blkTimes

    getDevAvailability :: HasCallStack => DevID -> State Schedule Double
    getDevAvailability devId = gets $ \mp -> 
      case mp `lookup` devId of
        []   -> 0
        t:ts -> assertPositive $ fst t 

    getCompletedTaskTime :: ThreadID -> State Schedule Double
    getCompletedTaskTime thrId = gets $ \mp -> 
      let rec [] = error $ "could not find completed task " ++ show thrId ++ "in schedule"
          rec (timeline:rest) = case find (\(_, mode) -> Just thrId == mode) timeline of 
                                  Just (t_task, _) -> t_task
                                  Nothing -> rec rest
      in rec (Map.elems mp) 

    assertPositive :: HasCallStack => Double -> Double
    assertPositive i = if i > 0 then i else error "assertPositive failed"

-- the lower the loss, the better the assigment
loss :: ScheduleParams -> Features -> Double
loss p f = 
  -- compute loss as a sum of weighted squares 
  sum [
      w_completed p * f_completed f'
    , w_priority p * f_priority f'
    ]
  where
    f' = (f + fromInteger 1) ** fromInteger 2 -- recenter and square

-- rescale features to [0,1] U {-Infinity, Infinity}
rescaleFeatures :: [Features] -> [Features]
rescaleFeatures fs = map (f_unop fromNaN . (\x -> (x - x_min) / (x_max - x_min))) fs
  where
    fromNaN a = if isNaN a then 0 else a
    x_min = f_nop (minimum . filter (not . isInfinite)) fs
    x_max = f_nop (maximum . filter (not . isInfinite)) fs
