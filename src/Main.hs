import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           MMZK.App
import           Model.TransportStatus
import           Opt
import           Opt.Utils
import           Prettyprinter
import           Service.Fetch
import           System.Directory
import           System.Exit

type App = App' AppConfig

getAPIKey :: IO String
getAPIKey = do
  contents <- map (second T.tail . T.breakOn "=") . T.lines <$> T.readFile ".env"
  case lookup "TFL_API_KEY" contents of
    Nothing -> do
      putStrLn "TfL API key is not found! There should be an .env file with TFL_API_KEY=..."
      exitWithHelp
    Just k  -> pure (T.unpack k)
{-# INLINE getAPIKey #-}

main :: IO ()
main = do
  opts   <- getOpts
  apiKey <- getAPIKey
  runAppSimple (AppConfig opts apiKey) app

app :: App ()
app = do
    opts <- asksConfig (.option)
    when opts.hasError do
      liftIO $ putStrLn (showErrors opts)
      exitWithHelp
    when opts.hasWarning $ liftIO (putStrLn (showWarnings opts))
    isHelpOn <- checkHelp
    unless isHelpOn $ do
      checkFile
      fetchLines

exitWithHelp :: MonadIO m => m a
exitWithHelp = liftIO $  do
  putStrLn helpMsg
  exitFailure

checkHelp :: App Bool
checkHelp = do
  opts <- asksConfig (.option)
  liftIO $ when opts.content.help (putStrLn helpMsg)
  pure opts.content.help 

checkFile :: App ()
checkFile = do
  opts <- asksConfig (.option)
  case opts.content.output of
    Nothing -> pure ()
    Just fp -> liftIO $ do
      dpe <- doesPathExist fp
      when dpe do
        putStrLn ("File " ++ fp ++ " already exists")
        exitWithHelp

fetchLines :: App ()
fetchLines = do
  opts <- asksConfig (.option)
  let oneFetch = do
        status <- maybe id filterOnLines opts.content.lines <$> fetchTFLStatus
        let result = vsep . punctuate "\n" $ pretty <$> status
        liftIO $ case opts.content.output of
          Nothing -> print result
          Just fp -> do
            writeFile fp (show result)
            putStrLn ("Output written to " ++ fp)
  case opts.content.timerSec of
    Nothing -> oneFetch
    Just n  -> forever $ do
      oneFetch
      liftIO $ threadDelay (fromIntegral n * 1000000)
