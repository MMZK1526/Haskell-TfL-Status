module Model.TransportStatus where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics
import           Model.LineName
import           Prettyprinter

data ValidityPeriod = ValidityPeriod
  { isNow :: Bool
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data DelayEntry = DelayEntry
  { statusSeverity            :: Int
  , statusSeverityDescription :: String
  , reason                    :: Maybe String
  , validityPeriods           :: [ValidityPeriod]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty DelayEntry where
  pretty :: DelayEntry -> Doc ann
  pretty entry = pretty entry.statusSeverityDescription
             <+> maybe mempty (parens . pretty) entry.reason
  {-# INLINE pretty #-}

data TransportStatusEntry = TransportStatusEntry
  { id           :: LineName
  , lineStatuses :: [DelayEntry]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TransportStatusEntry where
  pretty :: TransportStatusEntry -> Doc ann
  pretty entry = pretty entry.id <> ":"
             <+> align lineStatuses
    where
      lineStatuses = case entry.lineStatuses of
        [] -> "Good service"
        _  -> vsep (pretty <$> entry.lineStatuses)
  {-# INLINE pretty #-}

type TransportStatus = [TransportStatusEntry]

filterNow :: TransportStatus -> TransportStatus
filterNow = map \entry -> entry { lineStatuses = filter (any (.isNow) . (.validityPeriods)) entry.lineStatuses }
{-# INLINE filterNow #-}

filterOnLines :: [LineName] -> TransportStatus -> TransportStatus
filterOnLines ls = filter \entry -> entry.id `elem` ls
{-# INLINE filterOnLines #-}
