module FileIO 
  ( 
  -- * File I/O
    persist
  , persistRemote
  , retrieve
  , retrieveRemote
  -- * Stdin I/O
  , yn
  , ynCached
  , cmdOptions
  , mightNeedSudo
  ) where

    
import           System.Exit (ExitCode(..))
import           System.Process (system)
import           System.Directory (doesFileExist)
import           Data.Monoid
import           Data.Aeson
import           Data.List (find)
import qualified Data.ByteString.Lazy as BL
import           Text.Read (readMaybe)

import Constants

-- * File I/O

persist :: (ToJSON a) => String -> a -> IO ()
persist fname a = BL.writeFile (fname <> cache_ext) (encode a)

persistRemote :: (ToJSON a) => String -> a -> IO ()
persistRemote = persist -- TODO

retrieve :: (FromJSON a) => String -> IO (Either String a)
retrieve fname = do
  let f = fname <> cache_ext
  b <- doesFileExist f
  if b 
    then do
      cont <- BL.readFile (fname <> cache_ext)
      return $ case eitherDecode cont of 
        Left e -> 
          Left $ "Could not decode file {" <> fname <> "} due to error {" <> e <> "}"
        Right r -> Right r
    else return $ 
      Left $ "Could not find file {" <> fname <> "}"

retrieveRemote :: (FromJSON a) => String -> IO (Either String a)
retrieveRemote = retrieve -- TODO(anand)

-- * Stdin I/O

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

ynCached :: (ToJSON a, FromJSON a) => String -> IO a -> IO a
ynCached fname n = do
  f <- retrieve fname
  case f of 
    Left e -> do 
      putStrLn e 
      a <- n
      persist fname a
      return a
    Right r -> 
      yn ("Use cached " <> fname <> "?") (return r) n

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

mightNeedSudo :: String -> IO ExitCode
mightNeedSudo cmd = do
  e <- system cmd
  case e of 
    ExitFailure 1 -> do
      putStrLn $ show cmd <> " failed, trying with sudo"
      system $ "sudo " <> cmd
    _ -> return e

