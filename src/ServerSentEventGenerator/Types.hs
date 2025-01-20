module ServerSentEventGenerator.Types where

import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )

data Options = Options {
    eventId       :: Builder
  , retryDuration :: Int
  } deriving (Show)

instance Default Options where
  def = Options {
    eventId = def
  , retryDuration = cDefaultSseRetryDurationMs
  }

data SSE = SSE {
    sEventType     :: EventType
  , sDataLines     :: [Builder]
  , sOptions       :: Options
  } deriving Show

{- From the README.MD
ServerSentEventGenerator.send(
    eventType: EventType,
    dataLines: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }) -}

-- | A sum of the possible Datastar specific sse events that can be sent

data EventType =
    MergeFragments
  | RemoveFragments
  | MergeSignals
  | RemoveSignals
  | ExecuteScript
  deriving (Eq, Show)

instance Default EventType
  where def = MergeFragments

instance ToBuilder EventType where
  toBuilder MergeFragments   = cMergeFragments
  toBuilder RemoveFragments  = cRemoveFragments
  toBuilder MergeSignals     = cMergeSignals
  toBuilder RemoveSignals    = cRemoveSignals
  toBuilder ExecuteScript    = cExecuteScript

data MergeMode =
     Morph
   | Inner
   | Outer
   | Prepend
   | Append
   | Before
   | After
   | UpsertAttributes
   deriving (Eq, Show)

instance Default MergeMode
  where def = Morph

instance ToBuilder MergeMode where
   toBuilder Morph            = cMorph
   toBuilder Inner            = cInner
   toBuilder Outer            = cOuter
   toBuilder Prepend          = cPrepend
   toBuilder Append           = cAppend
   toBuilder Before           = cBefore
   toBuilder After            = cAfter
   toBuilder UpsertAttributes = cUpsertAttributes
