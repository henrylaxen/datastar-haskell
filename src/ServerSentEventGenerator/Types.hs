{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Types where

import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )
import Control.Exception

noSelector :: Selector Builder
noSelector = SEL (mempty :: Builder)

data Options = O {
    eventId       :: Builder
  , retryDuration :: Int
  } deriving (Show)

instance Default Options where
  def = O {
    eventId = def
  , retryDuration = cDefaultSseRetryDurationMs
  }

instance ToBuilder Options where
  toBuilder options =
    let
      withSSEdefault value defaultValue field =
        if value ==  defaultValue then mempty
           else field <> ": " <> toBuilder value
      a = withSSEdefault  (eventId options) mempty cEventId
      b = withSSEdefault  (retryDuration options) cDefaultSseRetryDurationMs cRetryDuration
    in mconcat . buildLines $ [a,b]


newtype Selector a = SEL {unSelector :: a}
  deriving (Show, Semigroup, Monoid, Eq)

instance ToBuilder a => ToBuilder (Selector a) where
  toBuilder = withDefault cSelector cDefaultSelector . toBuilder . unSelector
    
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

instance ToBuilder MergeMode where
   toBuilder Morph            = cMorph
   toBuilder Inner            = cInner
   toBuilder Outer            = cOuter
   toBuilder Prepend          = cPrepend
   toBuilder Append           = cAppend
   toBuilder Before           = cBefore
   toBuilder After            = cAfter
   toBuilder UpsertAttributes = cUpsertAttributes

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

instance ToBuilder FragmentOptions where
  toBuilder (FO a b) = mconcat . buildLines $ [
      withDefault cSettleDuration    cDefaultSettleDurationMs a
    , withDefault cUseViewTransition cDefaultUseViewTransition  b
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
