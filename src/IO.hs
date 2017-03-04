module IO 
  ( persist
  , retrieve
  , visualizeSchedule
  , uploadPrioritization
  , uploadSchedule
  , yn
  , ynCached
  , cmdOptions
  , enterParameters
  , mightNeedSudo
  , unfairCoin
  ) where

-- API methods

persist :: (Show a) => String -> a -> IO ()
persist fname a = writeFile (fname <> ".cache") (show a)

retrieve :: (Read a) => String -> IO a
retrieve fname = read <$> readFile (fname <> ".cache")

visualizeSchedule :: IO () 
visualizeSchedule = do
  b <- doesFileExist "schedule_vis.cache"
  if b 
    then do
      system "python visualize.py"
      url <- readFile "schedule_vis.url"
      system $ "google-chrome " <> url
      return ()
    else 
      putStrLn "Error: you need to run \"schedule\" first."

uploadPrioritization :: Prioritization -> IO ()
uploadPrioritization = undefined

uploadSchedule :: Schedule -> IO ()
uploadSchedule = undefined

-- CMDline metehods

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

-- misc

unfairCoin :: Double -> IO Bool
unfairCoin p = flip <$> getStdRandom (randomR (1,100))
  where
    flip r = r < (p * 100) 