module Model.LineName where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson.Types as JSON
import           Data.Text (Text)
import qualified Data.Text as T
import           Prettyprinter
import           Text.Read (readMaybe)

data LineName
  = Bakerloo
  | Central
  | Circle
  | DLR
  | District
  | Elizabeth
  | HammersmithAndCity
  | Jubilee
  | Metropolitan
  | Northern
  | Overground
  | Piccadilly
  | Tram
  | Victoria
  | WaterlooAndCity
  deriving (Eq, Ord, Enum, Bounded)

instance Read LineName where
  readsPrec :: Int -> String -> [(LineName, String)]
  readsPrec _ "bakerloo"          = [(Bakerloo, "")]
  readsPrec _ "central"           = [(Central, "")]
  readsPrec _ "circle"            = [(Circle, "")]
  readsPrec _ "dlr"               = [(DLR, "")]
  readsPrec _ "district"          = [(District, "")]
  readsPrec _ "elizabeth"         = [(Elizabeth, "")]
  readsPrec _ "hammersmith-city"  = [(HammersmithAndCity, "")]
  readsPrec _ "jubilee"           = [(Jubilee, "")]
  readsPrec _ "metropolitan"      = [(Metropolitan, "")]
  readsPrec _ "northern"          = [(Northern, "")]
  readsPrec _ "london-overground" = [(Overground, "")]
  readsPrec _ "piccadilly"        = [(Piccadilly, "")]
  readsPrec _ "tram"              = [(Tram, "")]
  readsPrec _ "victoria"          = [(Victoria, "")]
  readsPrec _ "waterloo-city"     = [(WaterlooAndCity, "")]
  readsPrec _ _                   = []
  {-# INLINE readsPrec #-}

instance Show LineName where
  show :: LineName -> String
  show = toID
  {-# INLINE show #-}

instance Pretty LineName where
  pretty :: LineName -> Doc ann
  pretty Bakerloo           = "Bakerloo"
  pretty Central            = "Central"
  pretty Circle             = "Circle"
  pretty DLR                = "DLR"
  pretty District           = "District"
  pretty Elizabeth          = "Elizabeth"
  pretty HammersmithAndCity = "Hammersmith & City"
  pretty Jubilee            = "Jubilee"
  pretty Metropolitan       = "Metropolitan"
  pretty Northern           = "Northern"
  pretty Overground         = "Overground"
  pretty Piccadilly         = "Piccadilly"
  pretty Tram               = "Tram"
  pretty Victoria           = "Victoria"
  pretty WaterlooAndCity    = "Waterloo & City"
  {-# INLINE pretty #-}

instance FromJSON LineName where
  parseJSON :: JSON.Value -> JSON.Parser LineName
  parseJSON
    = JSON.withText "LineName" \txt -> case readMaybe (T.unpack txt) of
      Just name -> pure name
      Nothing   -> fail $ "Invalid LineName " ++ T.unpack txt
  {-# INLINE parseJSON #-}

instance ToJSON LineName where
  toJSON :: LineName -> JSON.Value
  toJSON = JSON.String . T.pack . toID
  {-# INLINE toJSON #-}

toID :: LineName -> String
toID Bakerloo           = "bakerloo"
toID Central            = "central"
toID Circle             = "circle"
toID DLR                = "dlr"
toID District           = "district"
toID Elizabeth          = "elizabeth"
toID HammersmithAndCity = "hammersmith-city"
toID Jubilee            = "jubilee"
toID Metropolitan       = "metropolitan"
toID Northern           = "northern"
toID Overground         = "overground"
toID Piccadilly         = "piccadilly"
toID Tram               = "tram"
toID Victoria           = "victoria"
toID WaterlooAndCity    = "waterloo-city"
{-# INLINE toID #-}

parseLineName :: Text -> [LineName]
parseLineName txt = filter ((txt' `T.isInfixOf`) . T.pack . show) allLines
  where
    txt'     = T.toLower txt
    allLines = [minBound..maxBound]
