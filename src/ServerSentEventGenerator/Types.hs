module ServerSentEventGenerator.Types where

import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )
import Control.Exception

data Options = Options {
    eventId       :: Builder
  , retryDuration :: Int
  } deriving (Show)

instance Default Options where
  def = Options {
    eventId = def
  , retryDuration = cDefaultSseRetryDurationMs
  }

-- data SSE = SSE {
--     sEventType     :: EventType
--   , sDataLines     :: [Builder]
--   , sOptions       :: Options
--   } deriving Show

-- instance Default SSE where
--   def = SSE def [] def

{- From the README.MD
ServerSentEventGenerator.send(
    eventType: EventType,
    dataLines: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }) -}

-- data EventType =
--     Eevent
--   | Eretry
--   | Eid
--   | Edata
--   deriving Show

-- instance Default EventType where
--   def = Edata
-- instance ToBuilder EventType where
--   toBuilder Eevent = cEvent         <> cSpace
--   toBuilder Eretry = cRetryDuration <> cSpace
--   toBuilder Eid    = cEventId       <> cSpace
--   toBuilder Edata  = cData          <> cSpace

-- | A sum of the possible Datastar specific sse events that can be sent
-- data DatastarEventType =
--     MergeFragments
--   | RemoveFragments
--   | MergeSignals
--   | RemoveSignals
--   | ExecuteScript
--   deriving (Eq, Show)

-- instance Default DatastarEventType
--   where def = MergeFragments

-- instance ToBuilder DatastarEventType where
--   toBuilder MergeFragments   = cMergeFragments
--   toBuilder RemoveFragments  = cRemoveFragments
--   toBuilder MergeSignals     = cMergeSignals
--   toBuilder RemoveSignals    = cRemoveSignals
--   toBuilder ExecuteScript    = cExecuteScript

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

data FragmentOptions = FragmentOptions {
    settleDuration    :: Int
  , useViewTransition :: Bool
  } deriving (Show)

-- | the MergeFragments and RemoveFragment data types share these options

instance Default FragmentOptions where
  def = FragmentOptions {
    settleDuration     = cDefaultSettleDurationMs
  , useViewTransition  = cDefaultUseViewTransition
  }

instance ToBuilder FragmentOptions where
  toBuilder (FragmentOptions a b) = buildLine [
      dataWithDefault cSettleDuration    cDefaultSettleDurationMs a
    , dataWithDefault cUseViewTransition cDefaultUseViewTransition  b
    ]

buildLine :: [Builder] -> Builder
buildLine builders = (go mempty builders)
  where
    go acc [] = acc
    go acc (b:bs) = if b == mempty then go acc bs else b <> "\n" <> go acc bs

eventWithDefault :: (Eq a, ToBuilder a, ToBuilder b, ToBuilder c) => c -> b -> a -> a -> Builder
eventWithDefault eventType dStarEvent defaultValue value = if value == defaultValue
  then mempty
  else toBuilder eventType <> ": " <> toBuilder dStarEvent <> " " <>  toBuilder value

dataWithDefault :: (Eq a, ToBuilder a, ToBuilder b) => b -> a -> a -> Builder
dataWithDefault = eventWithDefault cData

t1 = buildLine ["a"]
t2 = toBuilder (def :: FragmentOptions)
t3 = toBuilder (FragmentOptions 1 True)
t4 = toBuilder (FragmentOptions 2 False)


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
