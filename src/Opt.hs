module Opt where

import           Data.Default
import           Data.List
import qualified Data.Text as T
import           Data.Word
import           MMZK.Read
import           Model.LineName
import           Opt.Utils
import           System.Console.GetOpt
import           System.Environment

data AppConfig = AppConfig
  { option :: AppOptions
  , apiKey :: String
  }

data AppOptionContent = AppOptionContent
  { lines    :: Maybe [LineName]
  , timerSec :: Maybe Word32
  , output   :: Maybe FilePath
  , help     :: Bool
  }

instance Default AppOptionContent where
  def :: AppOptionContent
  def = AppOptionContent Nothing Nothing Nothing False
  {-# INLINE def #-}

type AppOptions = OptionRec AppOptionContent

timerOption :: OptDescr (AppOptions -> AppOptions)
timerOption
  = Option ['t'] ["timer"] (ReqArg worker "SECONDS") "Periodically fetch the status"
  where
    worker arg opt = case readWord32 arg of
      Right n  -> case opt.content.timerSec of
        Nothing -> opt { content = opt.content { timerSec = Just n } }
        Just _  -> opt { warnings = "Duplicate timerSec" : opt.warnings }
      Left _   -> opt { errors = ("Invalid timer value " ++ show arg)
                               : opt.errors }

outputOption :: OptDescr (AppOptions -> AppOptions)
outputOption
  = Option ['o'] ["output"] (ReqArg worker "FILE") "Output file"
  where
    worker arg opt = case opt.content.output of
      Nothing -> opt { content = opt.content { output = Just arg } }
      Just _  -> opt { warnings = "Duplicate output" : opt.warnings }

helpOption :: OptDescr (AppOptions -> AppOptions)
helpOption
  = Option ['h'] ["help"] (NoArg worker) "Show help"
  where
    worker opt = opt { content = opt.content { help = True } }

lineArgs :: [String] -> AppOptions -> AppOptions
lineArgs = flip (foldl worker)
  where
    worker opt arg = case parseLineName (T.pack arg) of
      [] -> opt { errors = ("Unknown line " ++ arg) : opt.errors }
      ls -> case opt.content.lines of
        Nothing  -> opt { content = opt.content { lines = Just ls } }
        Just ls' ->
          opt { content = opt.content { lines = Just (nub $ ls ++ ls') } }

getOpts :: IO AppOptions
getOpts = parseOpt [timerOption, outputOption, helpOption] lineArgs <$> getArgs
{-# INLINE getOpts #-}

helpMsg :: String
helpMsg = unlines
  [ "TFL Status Fetcher\n"
  , usageInfo "Options:" [timerOption, outputOption, helpOption]
  , "By default, it returns the status of all lines."
  , "You may also provide a list of line names to filter on."
  , "For example, to get the status of Bakerloo and Central lines, run:\n"
  , "> cabal run tfl-status -- Bakerloo Central"
  , "\nYou may provide any name fragment, such as 'bak' or 'cen'."]
