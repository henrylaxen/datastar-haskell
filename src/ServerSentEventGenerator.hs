module ServerSentEventGenerator (

    EventType(..)
  , HttpVersion(..)
  , ToBuilder(..)
  , MergeMode(..)
  , Send(..)
  , MergeFragments
  , sseHeaders
  , send
  , sendPure
  , sendFragments
  , mergeFragments
  , removeFragment
  , mergeSignals
  , removeSignals
  , executeScript
  ) where

import           Constants
import Data.ByteString.Builder ( Builder )
import Data.Default ( Default(..) )
import ServerSentEventGenerator.Internal
--     ( ToBuilder(..), HttpVersion(..), maybeDefault, mapWithData, format )


-- $setup
-- >>> import           Data.Functor.Identity
-- >>> import           Data.Maybe
-- >>> import           Data.Text                 ( Text )
-- >>> import qualified Data.Text.Encoding        as T
-- >>> sampleDataLines = ["line 1", "line 2"] :: [Builder]

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
    optionEventId       :: Maybe Builder
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

data FragmentOptions = FragmentOptions {
    fragmentSettleDuration    :: Maybe Int
  , fragmentUseViewTransition :: Maybe Bool
  } deriving (Show)

-- | the MergeFragments and RemoveFragment data types share these options

instance Default FragmentOptions where
  def = FragmentOptions {
    fragmentSettleDuration     = Just cDefaultSettleDurationMs
  , fragmentUseViewTransition  = Just cDefaultFragmentsUseViewTransitions
  }

fragmentOptions :: FragmentOptions -> [Maybe Builder]
fragmentOptions frag =
  [ maybeDefault cSettleDuration      (fragmentSettleDuration frag)
  , maybeDefault cUseViewTransition   (fragmentUseViewTransition frag)
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
sendPure :: Send -> Builder
sendPure s = format builders
  where
    builders =
      [Just (withEvent (sendEventType s))]
      <> options (sendOptions s)
      <> mapWithData mempty (sendDataLines s)

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
sendFragments s = sendPure def {sendDataLines = map toBuilder s}

--------------------------------------- mergeFragments  ---------------------------------------
data MergeFragments = MergeFragments {
     mergeData              :: [Builder]
   , mergeSelector          :: Maybe Builder    -- > selector: "abc123"
   , mergeMode              :: Maybe MergeMode  -- > Morph is default
   , mergeFragmentOptions   :: FragmentOptions
   , mergeOptions           :: Options
   } deriving Show

instance Default MergeFragments where
  def                        = MergeFragments {
       mergeData              = []
    ,  mergeSelector          = Nothing
    ,  mergeMode              = Just Morph
    ,  mergeFragmentOptions   = def
    ,  mergeOptions           = def }

-- | convert a MergeFragments data type to a Builder, ready to be sent down the wire
--
-- Example
--
-- >>> :{
-- mergeFragments def {    mergeMode = Just UpsertAttributes
--                       , mergeData = sampleDataLines
--                       , mergeFragmentOptions = FragmentOptions (Just 500) (Just True)
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
      [ Just (withEvent EventMergeFragments) ]
      <> options (mergeOptions m)
      <> withDefaults
      <> mapWithData mempty (mergeData m)
    withDefaults :: [Maybe Builder]
    withDefaults =
      [  maybeDefault cMerge                      (mergeMode m)
      , ((<>) ("data: " <> cSelector <> " ")) <$> (mergeSelector m)
      ] <> fragmentOptions                        (mergeFragmentOptions m)


--------------------------------------- removeFragment  ---------------------------------------
{- From the README.MD
ServerSentEventGenerator.RemoveFragments(
    selector: string,
    options?: {
        settleDuration?: durationInMilliseconds,
        useViewTransition?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
-}
data RemoveFragment = RemoveFragment {
    removeSelector          :: Builder
  , removeFragmentOptions   :: FragmentOptions
  , removeOptions           :: Options
  } deriving Show

instance Default RemoveFragment where
  def                       = RemoveFragment {
    removeSelector          = bug (RemoveFragmentSelectorIsMissing "")
    , removeFragmentOptions = def
    , removeOptions         = def }


-- | convert a RemoveFragment data type to a Builder, ready to be sent down the wire
-- Note: the removeSelector field is required
--
-- Example
--
-- >>> removeFragment def {removeSelector = "id1", removeFragmentOptions = def {fragmentSettleDuration = Just 500}}
-- "event: datastar-remove-fragments\ndata: selector id1\ndata: settleDuration 500\ndata: useViewTransition false\n\n"
--
-- >>> removeFragment def
-- "*** Exception: RemoveFragmentSelectorIsMissing "the selector is required in RemoveFragment"

removeFragment :: RemoveFragment -> Builder
removeFragment r = format builders
  where
    builders =
      [Just    (withEvent EventRemoveFragments)]
      <> options (removeOptions r)
      <> [Just . ((<>) ("data: " <> cSelector <> " ")) $ (removeSelector r)]
      <> fragmentOptions                           (removeFragmentOptions r)
--------------------------------------- mergeSignals  ---------------------------------------
{- From the README.MD
ServerSentEventGenerator.MergeSignals(
    signals: string,
    options ?: {
        onlyIfMissing?: boolean,
        eventId?: string,
        retryDuration?: durationInMilliseconds
     }
 )
-}
data MergeSignals = MergeSignals {
    signalSelector          :: Builder
  , signalOnlyIfMissing     :: Maybe Bool
  , signalOptions           :: Options
  } deriving Show

instance Default MergeSignals where
  def                       = MergeSignals {
    signalSelector          = bug (SignalsSelectorIsMissing "")
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
-- "*** Exception: SignalsSelectorIsMissing "the selector is required in MergeSignals"

mergeSignals :: MergeSignals -> Builder
mergeSignals s = format builders
  where
    builders =
      [Just    (withEvent EventMergeSignals)]
      <> options (signalOptions s)
      <> [Just . ((<>) ("data: " <> cSignals <> " ")) $ (signalSelector s)]
      <> withDefaults
    withDefaults :: [Maybe Builder]
    withDefaults =
      [  maybeDefault cOnlyIfMissing    (signalOnlyIfMissing s) ]

--------------------------------------- removeSignals  ---------------------------------------
{- From the README.MD
ServerSentEventGenerator.RemoveSignals(
    paths: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
-}
data RemoveSignals = RemoveSignals {
    removeSignalsPath      :: [Builder]
  , removeSignalsOptions   :: Options
  } deriving Show

instance Default RemoveSignals where
  def                       = RemoveSignals {
    removeSignalsPath        = bug (RemoveSignalsPathIsMissing "")
  , removeSignalsOptions     = def }

-- | convert a RemoveSignals data type to a Builder, ready to be sent down the wire
-- Note: the removeSignalsPath field is required
--
-- Example
--
-- >>> removeSignals def {removeSignalsPath = ["user.name", "user.email"]}
-- "event: datastar-remove-signals\ndata: paths user.name\ndata: paths user.email\n\n"
--
-- >>> removeSignals def
-- "*** Exception: RemoveSignalsPathIsMissing "the path is required in RemoveSignals"
-- >>> removeSignals (RemoveSignals ["some.signal"] (Options Nothing (Just 10)))
-- "event: datastar-remove-signals\nretry: 10\ndata: paths some.signal\n\n"
-- >>> removeSignals (RemoveSignals [] (Options Nothing (Just 10)))
-- "*** Exception: RemoveSignalsPathIsEmpty "the path cannot be an empty list"
--

removeSignals :: RemoveSignals -> Builder
removeSignals r = format builders
  where
    builders =
       [Just    (withEvent EventRemoveSignals)]
       <> options (removeSignalsOptions r)
       <> if null (removeSignalsPath r)
             then bug (RemoveSignalsPathIsEmpty "")
             else mapWithData cPaths (removeSignalsPath r)

--------------------------------------- Execute Script  ---------------------------------------
{- From the README.MD
ServerSentEventGenerator.ExecuteScript(
    script: string,
    options?: {
        autoRemove?: boolean,
        attributes?: string,
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }
)
-}
data ExecuteScript = ExecuteScript {
    executeScriptJS      :: Builder
  , executeAttributes    :: [Builder]
  , executeAutoRemove    :: Maybe Bool
  , executeScriptOptions :: Options
  } deriving Show

instance Default ExecuteScript where
  def                    = ExecuteScript {
    executeScriptJS      = bug (ExecuteScriptIsMissing "")
  , executeAttributes    = [cDefaultAttributes]
  , executeAutoRemove    = Just cDefaultAutoRemove
  , executeScriptOptions = def }

-- | convert a ExecuteScript data type to a Builder, ready to be sent down the wire
-- Note: the executescriptPath field is required
--
-- Example
--
-- >>> executeScript (ExecuteScript "console.log('hi')" ["type text/javascript"] (Just False) def)
-- "event: datastar-execute-script\ndata: script console.log('hi')\ndata: attributes type text/javascript\ndata: autoRemove false\n\n"
--
-- >>> executeScript def
-- "*** Exception: ExecuteScriptIsMissing "the script is required in ExecuteScript"

executeScript :: ExecuteScript -> Builder
executeScript e = format builders
  where
    builders =
      [Just    (withEvent EventExecuteScript)]
      <> mapWithData "script" [executeScriptJS e]
      <> options (executeScriptOptions e)
      <> if null (executeAttributes e) || buildersMatch (executeAttributes e) [cDefaultAttributes]
            then mempty else mapWithData cAttributes (executeAttributes e)
      <> withDefaults
    withDefaults :: [Maybe Builder]
    withDefaults =
      [  maybeDefault cAutoRemove    (executeAutoRemove e) ]
