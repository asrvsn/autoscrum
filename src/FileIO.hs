module FileIO 
  ( 
  -- * File I/O
    persist
  , retrieve
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
import           Data.List (find)
import           Text.Read (readMaybe)

import Types

-- * File I/O

persist :: (Show a) => String -> a -> IO ()
persist fname a = writeFile (fname <> ".cache") (show a)

retrieve :: (Read a) => String -> IO a
retrieve fname = read <$> readFile (fname <> ".cache")

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

mightNeedSudo :: String -> IO ExitCode
mightNeedSudo cmd = do
  e <- system cmd
  case e of 
    ExitFailure 1 -> do
      putStrLn $ show cmd <> " failed, trying with sudo"
      system $ "sudo " <> cmd
    _ -> return e

