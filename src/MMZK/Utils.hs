module MMZK.Utils where

import           Data.ByteString (ByteString)
import qualified Network.HTTP.Client as H

(=:) :: a -> b -> (a, Maybe b)
a =: b = (a, Just b)
{-# INLINE (=:) #-}

(?) :: H.Request -> [(ByteString, Maybe ByteString)] -> H.Request
(?) = flip H.setQueryString
{-# INLINE (?) #-}
