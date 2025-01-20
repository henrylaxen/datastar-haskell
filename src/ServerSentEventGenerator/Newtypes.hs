
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Newtypes where
import Data.Default
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder

instance ToBuilderList DataLines where
  toBuilderList = unDataLines

instance ToBuilderList SignalsPath where
  toBuilderList = unSignalsPath

newtype SettleDuration = SettleDuration { unSettleDuration :: Int }
  deriving (Eq, Show, ToBuilder)
instance Default SettleDuration
  where def = SettleDuration cDefaultSettleDurationMs
instance DsCommand SettleDuration where
  dsCommand _ = cData <> ": " <> cSettleDuration

newtype UseViewTransition = UseViewTransition { unUseViewTransition :: Bool }
  deriving (Eq, Show, ToBuilder)
instance Default UseViewTransition
  where def = UseViewTransition cDefaultUseViewTransition
instance DsCommand UseViewTransition where
  dsCommand _ = cData <> ": " <> cUseViewTransition

newtype EventId = EventId { unEventId :: Builder }
  deriving (Eq, Show, ToBuilder)
instance Default EventId
  where def = EventId cDefaultEventId
instance DsCommand EventId where
  dsCommand _ = cEventId <> ": "

newtype RetryDuration = RetryDuration { unRetryDuration :: Int }
  deriving (Eq, Show, ToBuilder)
instance Default RetryDuration
  where def = RetryDuration cDefaultSseRetryDurationMs
instance DsCommand RetryDuration where
  dsCommand _ = cRetryDuration <> ": "

newtype DataLines = DataLines { unDataLines :: [Builder] }
  deriving (Eq, Show, ToBuilder)
instance Default DataLines
  where def = DataLines mempty
instance DsCommand DataLines where
  dsCommand _ = cData <> ": "

newtype Selector = Selector { unSelector :: Builder }
  deriving (Eq, Show, ToBuilder)
instance Default Selector
  where def = Selector cDefaultSelector
instance DsCommand Selector where
  dsCommand _ = cData <> ": " <> cSelector

newtype OnlyIfMissing = OnlyIfMissing { unOnlyIfMissing :: Bool }
  deriving (Eq, Show, ToBuilder)
instance Default OnlyIfMissing
  where def = OnlyIfMissing cDefaultOnlyIfMissing
instance DsCommand OnlyIfMissing where
  dsCommand _ = cData <> ": " <> cOnlyIfMissing

newtype Signals = Signals { unSignals :: Builder }
  deriving (Eq, Show, ToBuilder)
instance Default Signals
  where def = Signals mempty
instance DsCommand Signals where
  dsCommand _ = cData <> ": " <> cSignals

newtype SignalsPath = SignalsPath { unSignalsPath :: [Builder] }
  deriving (Eq, Show, ToBuilder)
instance Default SignalsPath
  where def = SignalsPath mempty
instance DsCommand SignalsPath where
  dsCommand _ = cData <> ": " <> cPaths

newtype Script = Script { unScript :: Builder }
  deriving (Eq, Show, ToBuilder)
instance Default Script
  where def = Script mempty
instance DsCommand Script where
  dsCommand _ = cData <> ": " <> cScript

newtype Attributes = Attributes { unAttributes :: [Builder] }
  deriving (Eq, Show, ToBuilder)
instance Default Attributes
  where def = Attributes [cDefaultAttributes]
instance DsCommand Attributes where
  dsCommand _ = cData <> ": " <> cAttributes

newtype AutoRemove = AutoRemove { unAutoRemove :: Bool }
  deriving (Eq, Show, ToBuilder)
instance Default AutoRemove
  where def = AutoRemove cDefaultAutoRemove
instance DsCommand AutoRemove where
  dsCommand _ = cData <> ": " <> cAutoRemove
