module ServerSentEventGenerator (

    EventType(..)
  , HttpVersion(..)
  , ToBuilder(..)
  , MergeMode(..)
  , Send(..)
  , MergeFragments
  , sseHeaders
  , send
  , sendFragments
  , mergeFragments
  , removeFragment
  , mergeSignals
  , removeSignal
  ) where

import           Constants
import Data.ByteString.Builder ( Builder )
import Data.Default ( Default(..) )
import ServerSentEventGenerator.Internal
--     ( ToBuilder(..), HttpVersion(..), maybeDefault, mapWithData, format )
import Control.Exception

-- $setup
-- >>> import           Data.Functor.Identity
-- >>> import           Data.Maybe
-- >>> import           Data.Text                 ( Text )
-- >>> import qualified Data.Text.Encoding        as T
-- >>> sampleDataLines = ["line 1", "line 2"] :: [Builder]

-- import qualified Data.ByteString.Lazy      as B
-- import           Data.ByteString.Lazy.UTF8
-- import qualified Data.Text                 as T

-- | returns the Http header for an SSE depending
--   on the Http version you are using. Note: you will
--   have to implement an instance of the HttpVersion class
--   for whichever web server you are using
--
-- Example:
--
-- >>> builderToString . runIdentity $ sseHeaders
-- "Cache-control: no-cache\nContent-type: text/event-stream\nConnection: keep-alive\n"


sseHeaders :: HttpVersion m => m Builder
sseHeaders = do
  b <- isHttpVersion1_1
  return $ if b then sseHeaders1_1 else sseHeaders2
  where
    sseHeaders2 = "Cache-control: no-cache\nContent-type: text/event-stream\n"
    sseHeaders1_1 = sseHeaders2 <> "Connection: keep-alive\n"

--     withDefaults :: [Maybe Builder]
--     withDefaults = 
--       [  maybeDefault  (mergeMode m)
--       ,  maybeDefault  (mergeUseUiewTransition m)
--       ,  maybeDefault  (mergeUseUiewTransition m) ]

data Options = Options {
    optionEventId :: Maybe Builder
  , optionRetryDuration :: Maybe Int
  } deriving (Show)

instance Default Options where
  def = Options {
    optionEventId = Nothing
  , optionRetryDuration = Just cDefaultSseRetryDurationMs
    }

-- | All server sent events can contain and Event Id and a Retry Duration as an option

options ::  Options -> [Maybe Builder]
options opt =
  [
    ("id: " <>)  <$> optionEventId opt,
    if optionRetryDuration opt == optionRetryDuration def 
    then Nothing
    else ((<>) "retry: " . toBuilder) <$> optionRetryDuration opt
  ]

-- | A sum of the possible Datastar specific sse events that can be sent

data EventType =
    EventMergeFragments
  | EventRemoveFragments
  | EventMergeSignals
  | EventRemoveSignals
  | EventExecuteScript
  deriving (Eq, Show)

instance Default EventType
  where def = EventMergeFragments

instance ToBuilder EventType where
  toBuilder EventMergeFragments   = cMergeFragments
  toBuilder EventRemoveFragments  = cRemoveFragments
  toBuilder EventMergeSignals     = cMergeSignals
  toBuilder EventRemoveSignals    = cRemoveSignals
  toBuilder EventExecuteScript    = cExecuteScript

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

{- From the README.MD
ServerSentEventGenerator.send(
    eventType: EventType,
    dataLines: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }) -}

data Send = Send {
    sendEventType     :: EventType
  , sendDataLines     :: [Builder]
  , sendOptions       :: Options
  } deriving Show

--------------------------------------- Functions Start Here ---------------------------------------

instance Default Send where
  def = Send EventMergeFragments [] def


-- | convert a Send data type to a Builder, ready to be sent down the wire
--
-- Example
--
-- >>> send $ Send EventMergeFragments sampleDataLines (def {optionEventId = Just "abc123"})
-- "event: datastar-merge-fragments\nid: abc123\ndata: line 1\ndata: line 2\n\n"
--
-- >>> send (def {sendDataLines = sampleDataLines})
-- "event: datastar-merge-fragments\ndata: line 1\ndata: line 2\n\n"

--------------------------------------- send  ---------------------------------------
send :: Send -> Builder
send s = format builders
  where
    builders = 
      [Just ("event: " <> toBuilder (sendEventType s))]
      <> options (sendOptions s)
      <> mapWithData (sendDataLines s)

-- | A convenience function that takes a list of ByteString/String/Text and
--   outputs a Builder, assuming the rest of the Send Data Type fields are
--   the defaults.
--
-- Example
--
-- >>> sendFragments (["l1", "l2"] :: [String])
-- "event: datastar-merge-fragments\ndata: l1\ndata: l2\n\n"
--
-- >>> sendFragments (["l1", "l2"] :: [Text])
-- "event: datastar-merge-fragments\ndata: l1\ndata: l2\n\n"

{- From the README.MD

ServerSentEventGenerator.MergeFragments(
    fragments: string,
    options?: {
        selector?: string,
        mergeMode?: FragmentMergeMode,
        settleDuration?: durationInMilliseconds,
        useViewTransition?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
     }
 )
| Mode             | Description                                             |
|------------------|---------------------------------------------------------|
| morph            | Use idiomorph to merge the fragment into the DOM        |
| inner            | Replace the innerHTML of the selector with the fragment |
| outer            | Replace the outerHTML of the selector with the fragment |
| prepend          | Prepend the fragment to the selector                    |
| append           | Append the fragment to the selector                     |
| before           | Insert the fragment before the selector                 |
| after            | Insert the fragment after the selector                  |
| upsertAttributes | Update the attributes of the selector with the fragment |

-}

--------------------------------------- sendFragments  ---------------------------------------
sendFragments :: ToBuilder a => [a] -> Builder
sendFragments s = send def {sendDataLines = map toBuilder s}

--------------------------------------- mergeFragments  ---------------------------------------
data MergeFragments = MergeFragments {
     mergeData              :: [Builder]
   , mergeSelector          :: Maybe Builder    -- > selector: "abc123"
   , mergeMode              :: Maybe MergeMode  -- > Morph is default
   , mergeSettleDuration    :: Maybe Int
   , mergeUseViewTransition :: Maybe Bool
   , mergeOptions           :: Options
   } deriving Show

instance Default MergeFragments where
  def                        = MergeFragments {
       mergeData              = []
    ,  mergeSelector          = Nothing
    ,  mergeMode              = Just Morph
    ,  mergeSettleDuration    = Just cDefaultSettleDurationMs
    ,  mergeUseViewTransition = Just False
    ,  mergeOptions           = def }

-- | convert a MergeFragments data type to a Builder, ready to be sent down the wire
--
-- Example
--
-- >>> :{
-- mergeFragments def {  mergeMode = Just UpsertAttributes
--                       , mergeData = sampleDataLines
--                       , mergeSettleDuration = Just 500
--                       , mergeUseViewTransition = Just True
--                       , mergeSelector = Just "#id"}
-- :}
-- "event: datastar-merge-fragments\ndata: merge upsertAttributes\ndata: selector #id\ndata: settleDuration 500\ndata: useViewTransition true\ndata: line 1\ndata: line 2\n\n"
--
-- >>> send (def {sendDataLines = sampleDataLines})
-- "event: datastar-merge-fragments\ndata: line 1\ndata: line 2\n\n"

mergeFragments :: MergeFragments -> Builder
mergeFragments m = format builders
  where
    builders = 
      [ Just ("event: " <> toBuilder EventMergeFragments) ]
      <> options (mergeOptions m)
      <> withDefaults
      <> mapWithData (mergeData m)
    withDefaults :: [Maybe Builder]
    withDefaults = 
      [  maybeDefault cMerge                      (mergeMode m)
      , ((<>) ("data: " <> cSelector <> " ")) <$> (mergeSelector m)
      ,  maybeDefault cSettleDuration             (mergeSettleDuration m)
      ,  maybeDefault cUseViewTransition          (mergeUseViewTransition m)
      ]                            

--------------------------------------- removeFragment  ---------------------------------------
data RemoveFragment = RemoveFragment {
    removeSelector          :: Builder
  , removeSettleDuration    :: Maybe Int
  , removeUseViewTransition :: Maybe Bool
  , removeOptions           :: Options
  } deriving Show

instance Default RemoveFragment where
  def                       = RemoveFragment {
    removeSelector          = throw (RemoveFragmentSelectorIsMissing "the selector is required in RemoveFragment")
  , removeSettleDuration    = Just cDefaultSettleDurationMs
  , removeUseViewTransition = Just False
  , removeOptions           = def }


-- | convert a RemoveFragment data type to a Builder, ready to be sent down the wire
-- Note: the removeSelector field is required
--
-- Example
--
-- >>> removeFragment def {removeSelector = "id1", removeSettleDuration = Just 500  }
-- "event: datastar-remove-fragments\ndata: selector id1\ndata: settleDuration 500\n\n"
--
-- >>> removeFragment def
-- "*** Exception: RemoveFragmentSelectorIsMissing "the selector is required in RemoveFragment"

removeFragment :: RemoveFragment -> Builder
removeFragment r = format builders
  where
    builders =
      [Just    ("event: " <> toBuilder EventRemoveFragments)]
      <> options (removeOptions r)
      <> [Just . ((<>) ("data: " <> cSelector <> " ")) $ (removeSelector r)]
      <> withDefaults
    withDefaults :: [Maybe Builder]
    withDefaults = 
      [  maybeDefault cSettleDuration    (removeSettleDuration r)
      ,  maybeDefault cUseViewTransition (removeUseViewTransition r) ]

--------------------------------------- mergeSignals  ---------------------------------------
data MergeSignals = MergeSignals {
    signalSelector          :: Builder
  , signalOnlyIfMissing     :: Maybe Bool
  , signalOptions           :: Options
  } deriving Show

instance Default MergeSignals where
  def                       = MergeSignals {
    signalSelector          = throw (SignalsSelectorIsMissing "the selector is required in SignalFragment")
  , signalOnlyIfMissing     = Just cDefaultOnlyIfMissing
  , signalOptions           = def }

-- | convert a MergeSignals data type to a Builder, ready to be sent down the wire
-- Note: the signalSelector field is required
--
-- Example
--
-- >>> mergeSignals def {signalSelector = "{'key': 'value'}",  signalOnlyIfMissing = Just True}
-- "event: datastar-merge-signals\ndata: signals {'key': 'value'}\ndata: onlyIfMissing true\n\n"
--
-- >>> mergeSignals def
-- "*** Exception: SignalsSelectorIsMissing "the selector is required in SignalFragment"

mergeSignals :: MergeSignals -> Builder
mergeSignals s = format builders
  where
    builders =
      [Just    ("event: " <> toBuilder EventMergeSignals)]
      <> options (signalOptions s)
      <> [Just . ((<>) ("data: " <> cSignals <> " ")) $ (signalSelector s)]
      <> withDefaults
    withDefaults :: [Maybe Builder]
    withDefaults = 
      [  maybeDefault cOnlyIfMissing    (signalOnlyIfMissing s) ]

--------------------------------------- removeSignal  ---------------------------------------
data RemoveSignal = RemoveSignal {
    removeSignalPath      :: Builder
  , removeSignalOptions   :: Options
  } deriving Show

instance Default RemoveSignal where
  def                       = RemoveSignal {
    removeSignalPath        = throw (RemoveSignalPathIsMissing "the selector is required in RemoveSignal")
  , removeSignalOptions     = def }

-- | convert a RemoveSignal data type to a Builder, ready to be sent down the wire
-- Note: the removeSelector field is required
--
-- Example
--
-- >>> removeSignal def {removeSelector = "id1", removeSettleDuration = Just 500  }
-- "event: datastar-remove-signals\ndata: selector id1\ndata: settleDuration 500\n\n"
--
-- >>> removeSignal def
-- "*** Exception: RemoveSignalSelectorIsMissing "the selector is required in RemoveSignal"

removeSignal :: RemoveSignal -> Builder
removeSignal r = format builders
  where
    builders =
      [Just    ("event: " <> toBuilder EventRemoveSignals)]
      <> options (removeSignalOptions r)
      <> [Just . ((<>) ("data: " <> cPaths <> " ")) $ (removeSignalPath r)]

---------------------------------------   ---------------------------------------


data ServerSentEventGeneratorExceptions =
    RemoveFragmentSelectorIsMissing String
  | SignalsSelectorIsMissing String
  | RemoveSignalPathIsMissing String
  deriving Show
instance Exception ServerSentEventGeneratorExceptions

