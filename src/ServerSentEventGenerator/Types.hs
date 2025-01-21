{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Types where

import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )
import Control.Exception

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
    in buildLines [a,b]


-- t1 = buildLines []
-- t2 = toBuilder (def :: Options)
-- t3 = toBuilder (Options "ab123" 1)
-- t4 = toBuilder (Options "" 1)
-- t5 = toBuilder (Options "ab123" cDefaultSseRetryDurationMs)
-- t = mapM_ sp [t1,t2,t3,t4,t5]


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

newtype Selector a = SEL {unSelector :: a}
  deriving (Show, Semigroup, Monoid, Eq)

instance ToBuilder a => ToBuilder (Selector a) where
  toBuilder = withDefault cSelector cDefaultSelector . toBuilder . unSelector
    
data EventType =
    MergeFragments
  | RemoveFragments
  | MergeSignals
  | RemoveSignals
  | ExecuteScript
  deriving (Eq, Show)

noSelector :: Selector Builder
noSelector = SEL (mempty :: Builder)

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
  toBuilder (FO a b) = buildLines [
      withDefault cSettleDuration    cDefaultSettleDurationMs a
    , withDefault cUseViewTransition cDefaultUseViewTransition  b
    ]

-- t1 = buildLines ["a"]
-- t2 = toBuilder (def :: FragmentOptions)
-- t3 = toBuilder (FragmentOptions 1 True)
-- t4 = toBuilder (FragmentOptions 2 False)


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
