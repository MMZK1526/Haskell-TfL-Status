module MMZK.Read
  ( readWord32
  ) where

import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.Word

digit2Num :: Num a => Char -> Maybe a
digit2Num c = guard (isDigit c) $> fromIntegral (ord c - ord '0')
{-# INLINE digit2Num #-}

two32Div10 :: Word32
two32Div10 = 429496729

readWord32 :: String -> Either String Word32
readWord32 ""  = Left "MMZK.Read.readEither: parse error"
readWord32 str = foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two32Div10 || (num == two32Div10 && digit <= 5))
        $ Left "MMZK.Read.readEither: Word32 out of range"
      pure $ num * 10 + digit
