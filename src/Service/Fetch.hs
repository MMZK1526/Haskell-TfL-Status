module Service.Fetch where

import           Data.String
import           GHC.Records
import           MMZK.App
import           MMZK.Utils
import           Model.TransportStatus
import qualified Network.HTTP.Client as H

fetchTFLStatus :: HasField "apiKey" config String => App' config TransportStatus
fetchTFLStatus = do
  apiKey <- asksConfig (getField @"apiKey")
  resp   <- httpJSON @TransportStatus
    $ "https://api.tfl.gov.uk/Line/Mode/tube,dlr,overground,elizabeth-line,tram/Status"
    ? [ "detail"  =: "true", "app_key" =: fromString apiKey ]
  pure . filterNow $ H.responseBody resp
