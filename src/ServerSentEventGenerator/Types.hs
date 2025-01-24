{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Types where

import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.Text
import Data.Default
import Control.Exception

data Options = O {
    eventId       :: Text
  , retryDuration :: Int
  } deriving (Show)

instance Default Options where
  def = O {
    eventId = def
  , retryDuration = cDefaultSseRetryDurationMs
  }

instance ToText Options where
  toText options =
    let
      withSSEdefault value defaultValue field =
        if value ==  defaultValue then mempty
           else field <> ": " <> toText value
      a = withSSEdefault  (eventId options) mempty cEventId
      b = withSSEdefault  (retryDuration options) cDefaultSseRetryDurationMs cRetryDuration
    in mconcat . buildLines $ [a,b]


newtype Selector = SEL {unSelector :: Text}
  deriving (Show, Semigroup, Monoid, Eq)

instance Default Selector where
  def = SEL ""

instance ToText Selector where
  toText = withDefault cSelector cDefaultSelector . unSelector
    
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

instance ToText EventType where
  toText MergeFragments   = cMergeFragments
  toText RemoveFragments  = cRemoveFragments
  toText MergeSignals     = cMergeSignals
  toText RemoveSignals    = cRemoveSignals
  toText ExecuteScript    = cExecuteScript

-- | A sum of the possible Datastar specific merge modes that can be sent

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

instance ToText MergeMode where
   toText Morph            = cMorph
   toText Inner            = cInner
   toText Outer            = cOuter
   toText Prepend          = cPrepend
   toText Append           = cAppend
   toText Before           = cBefore
   toText After            = cAfter
   toText UpsertAttributes = cUpsertAttributes

data FragmentOptions = FO {
    settleDuration    :: Int
  , useViewTransition :: Bool
  } deriving (Show)

-- | the MergeFragments and RemoveFragment data types share these options

instance Default FragmentOptions where
  def = FO {
    settleDuration     = cDefaultSettleDurationMs
  , useViewTransition  = cDefaultUseViewTransition
  }

instance ToText FragmentOptions where
  toText (FO a b) = mconcat . buildLines $ [
      withDefault cSettleDuration    (toText cDefaultSettleDurationMs)   (toText a)
    , withDefault cUseViewTransition (toText cDefaultUseViewTransition)  (toText b)
    ]

instance Show ServerSentEventGeneratorExceptions where
 show BuildLineDataIMissing           = "buildLine was call with nothing to build"
 show RemoveFragmentSelectorIsMissing = "The selector field is required in RemoveFragment"
 show SignalsSelectorIsMissing        = "The selector field is required in MergeSignals"
 show RemoveSignalsPathIsMissing      = "The path field is required in RemoveSignals"
 show RemoveSignalsPathIsEmpty        = "The path field cannot be an empty list"
 show ExecuteScriptIsMissing          = "The script field is required in ExecuteScript"

bug :: Exception e => e -> a
bug = throw

data ServerSentEventGeneratorExceptions =
   BuildLineDataIMissing 
 | RemoveFragmentSelectorIsMissing 
 | SignalsSelectorIsMissing        
 | RemoveSignalsPathIsMissing      
 | RemoveSignalsPathIsEmpty        
 | ExecuteScriptIsMissing          

instance Exception ServerSentEventGeneratorExceptions
