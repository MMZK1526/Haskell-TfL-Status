module Opt.Utils where

import           Data.Default
import           Data.Function
import           GHC.Records
import           System.Console.GetOpt

data OptionRec a = OptionRec
  { errors   :: [String]
  , warnings :: [String]
  , content  :: a }

instance Default a => Default (OptionRec a) where
  def :: Default a => OptionRec a
  def = OptionRec [] [] def
  {-# INLINE def #-}

instance HasField "hasError" (OptionRec a) Bool where
  getField :: OptionRec a -> Bool
  getField = not . null . (.errors)
  {-# INLINE getField #-}

instance HasField "hasWarning" (OptionRec a) Bool where
  getField :: OptionRec a -> Bool
  getField = not . null . (.warnings)
  {-# INLINE getField #-}

showErrors :: OptionRec a -> String
showErrors opt = unlines $ "Errors:" : reverse opt.errors
{-# INLINE showErrors #-}

showWarnings :: OptionRec a -> String
showWarnings opt = unlines $ "Warnings:" : reverse opt.warnings
{-# INLINE showWarnings #-}

addError :: String -> OptionRec a -> OptionRec a
addError err opt = opt { errors = err : opt.errors }
{-# INLINE addError #-}

addErrors :: [String] -> OptionRec a -> OptionRec a
addErrors errs opt = opt { errors = errs ++ opt.errors }
{-# INLINE addErrors #-}

addWarning :: String -> OptionRec a -> OptionRec a
addWarning warn opt = opt { warnings = warn : opt.warnings }
{-# INLINE addWarning #-}

addWarnings :: [String] -> OptionRec a -> OptionRec a
addWarnings warns opt = opt { warnings = warns ++ opt.warnings }
{-# INLINE addWarnings #-}

parseOpt :: Default a => [OptDescr (OptionRec a -> OptionRec a)]
         -> ([String] -> OptionRec a -> OptionRec a)
         -> [String] -> OptionRec a
parseOpt options argWorker args
  = argWorker nonOptions . addErrors errors $ foldl (&) def actions
  where
    (actions, nonOptions, errors) = getOpt RequireOrder options args
