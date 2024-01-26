module MMZK.App where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson (FromJSON)
import qualified Data.Aeson as JSON
import           Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H

data AppEnv config = AppEnv
  { httpManager :: H.Manager
  , config      :: config
  }

type App' config = ReaderT (AppEnv config) IO

runApp :: IO H.Manager -> config -> App' config a -> IO a
runApp ioManager config app = ioManager >>= runReaderT app . (`AppEnv` config)
{-# INLINE runApp #-}

runApp' :: IO H.Manager -> App' () a -> IO a
runApp' = flip runApp ()
{-# INLINE runApp' #-}

runAppSimple :: config -> App' config a -> IO a
runAppSimple = runApp (H.newManager H.tlsManagerSettings)
{-# INLINE runAppSimple #-}

runAppSimple' :: App' () a -> IO a
runAppSimple' = runAppSimple ()
{-# INLINE runAppSimple' #-}

withResponse :: H.Request -> (H.Response H.BodyReader -> IO a) ->  App' config a
withResponse req f = do
  manager <- asks (.httpManager)
  liftIO $ H.withResponse req manager f
{-# INLINE withResponse #-}

httpLbs :: H.Request ->  App' config (H.Response ByteString)
httpLbs req = do
  manager <- asks (.httpManager)
  liftIO $ H.httpLbs req manager
{-# INLINE httpLbs #-}

httpJSON :: FromJSON a => H.Request ->  App' config (H.Response a)
httpJSON req = do
  resp <- httpLbs req
  case JSON.eitherDecode (H.responseBody resp) of
    Left err -> fail err
    Right a  -> pure (resp { H.responseBody = a })
{-# INLINE httpJSON #-}

httpJSONDyn :: H.Request ->  App' config (H.Response JSON.Value)
httpJSONDyn = httpJSON
{-# INLINE httpJSONDyn #-}

askConfig :: App' config config
askConfig = asks (.config)
{-# INLINE askConfig #-}

asksConfig :: (config -> a) -> App' config a
asksConfig f = asks (f . (.config))
{-# INLINE asksConfig #-}
