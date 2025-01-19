module ServerSentEventGenerator (

    HttpVersion(..)
  , Sender(..)
  , ToBuilder(..)
  , Options(..)
  , FragmentOptions(..)
  , EventType(..)
  , MergeMode(..)
  , Send(..)
  , MergeFragments(..)
  , RemoveFragment(..)
  , MergeSignals(..)
  , RemoveSignals(..)
  , ExecuteScript(..)
  , sseHeaders
  , sendPure
  , sendFragments
  , mergeFragments
  , removeFragment
  , mergeSignals
  , removeSignals
  , executeScript
  ) where

import ServerSentEventGenerator.Constants
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )
import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Newtypes
import ServerSentEventGenerator.Class

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
-- >>> runIdentity $ sseHeaders
-- "Cache-control: no-cache\nContent-type: text/event-stream\nConnection: keep-alive\n"

sseHeaders :: HttpVersion m => m Builder
sseHeaders = do
  b <- isHttpVersion1_1
  return $ if b then sseHeaders1_1 else sseHeaders2
  where
    sseHeaders2 = "Cache-control: no-cache\nContent-type: text/event-stream\n"
    sseHeaders1_1 = sseHeaders2 <> "Connection: keep-alive\n"

data Options = Options {
    eventId       :: Builder
  , retryDuration :: RetryDuration
  } deriving (Show)

instance Default Options where
  def = Options {
    eventId = mempty
  , retryDuration = def
  }

-- | All server sent events can contain and Event Id and a Retry Duration as an option
--   This works, because of the options in opt are equal to their defaults, they will
--   later be removed from the output

options ::  Options -> [Maybe Builder]
options opt =
  [
    withDefault "id:"    (eventId opt)
  , withDefault "retry:" (retryDuration opt)
  ]

data FragmentOptions = FragmentOptions {
    settleDuration    :: SettleDuration
  , useViewTransition :: UseViewTransition
  } deriving (Show)

-- | the MergeFragments and RemoveFragment data types share these options

instance Default FragmentOptions where
  def = FragmentOptions {
    settleDuration     = def
  , useViewTransition  = def
  }

fragmentOptions :: FragmentOptions -> [Maybe Builder]
fragmentOptions frag =
  [ withDefault cSettleDuration       (settleDuration frag)
  , withDefault cUseViewTransition    (useViewTransition frag)
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


-- format :: [Maybe Builder] -> Builder
-- format x = (withLineFeeds . catMaybes) x <> "\n"

--------------------------------------- Functions Start Here ---------------------------------------

instance Default Send where
  def = Send EventMergeFragments [] def

-- | convert a Send data type to a Builder, ready to be sent down the wire
--
-- Example
--
-- >>> sendPure $ Send EventMergeFragments sampleDataLines (def {eventId = Just "abc123"})
-- "event: datastar-merge-fragments\nid: abc123\ndata: line 1\ndata: line 2\n\n"
--
-- >>> sendPure (def {sendDataLines = sampleDataLines})
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
   , mergeSelector          :: Selector
   , mergeMode              :: MergeMode  -- > Morph is default
   , mergeFragmentOptions   :: FragmentOptions
   , mergeOptions           :: Options
   } deriving Show

instance Default MergeFragments where
  def                        = MergeFragments {
       mergeData              = []
    ,  mergeSelector          = def
    ,  mergeMode              = def
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
-- >>> sendPure (def {sendDataLines = sampleDataLines})
-- "event: datastar-merge-fragments\ndata: line 1\ndata: line 2\n\n"

mergeFragments :: MergeFragments -> Builder
mergeFragments m = format builders
  where
    builders =
      [ Just (withEvent EventMergeFragments) ]
      <> options (mergeOptions m)
      <> withDefaults
      <> mapWithData mempty (mergeData m)
    withDefaults =
      [  withDefault cMerge          (mergeMode m)
      ,  withDefault cSelector       (mergeSelector m)
      ] <> fragmentOptions           (mergeFragmentOptions m)



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
    removeSelector          :: Selector
  , removeFragmentOptions   :: FragmentOptions
  , removeOptions           :: Options
  } deriving Show

instance Default RemoveFragment where
  def                       = RemoveFragment {
      removeSelector        = bug RemoveFragmentSelectorIsMissing
    , removeFragmentOptions = def
    , removeOptions         = def }

-- | convert a RemoveFragment data type to a Builder, ready to be sent down the wire
-- Note: the removeSelector field is required
--
-- Example
--
-- >>> removeFragment def {removeSelector = "id1", removeFragmentOptions = def {settleDuration = Just 500}}
-- "event: datastar-remove-fragments\ndata: selector id1\ndata: settleDuration 500\ndata: useViewTransition false\n\n"
--
-- >>> removeFragment def
-- "*** Exception: RemoveFragmentSelectorIsMissing "the selector is required in RemoveFragment"

removeFragment :: RemoveFragment -> Builder
removeFragment r = format builders
  where
    builders :: [Maybe Builder]
    builders =
      [Just  (withEvent EventRemoveFragments)]
      <> options                                                 (removeOptions r)
      <> [withRequired RemoveFragmentSelectorIsMissing cSelector (removeSelector r)]
      <> fragmentOptions                                         (removeFragmentOptions r)
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
  , signalOnlyIfMissing     :: OnlyIfMissing  
  , signalOptions           :: Options
  } deriving Show

instance Default MergeSignals where
  def                       = MergeSignals {
    signalSelector          = def
  , signalOnlyIfMissing     = def
  , signalOptions           = def }

-- | convert a MergeSignals data type to a Builder, ready to be sent down the wire
-- Note: the signalSelector field is required
--
-- Example
--
-- >>> mergeSignals def {signalSelector = "{'key': 'value'}",  signalOnlyIfMissing =  OnlyIfMissing True}
-- "event: datastar-merge-signals\ndata: signals {'key': 'value'}\ndata: onlyIfMissing true\n\n"
--
-- >>> mergeSignals def
-- "*** Exception: SignalsSelectorIsMissing "the selector is required in MergeSignals"

mergeSignals :: MergeSignals -> Builder
mergeSignals s = format builders
  where
    builders :: [Maybe Builder]
    builders = 
        (Just    (withEvent EventMergeSignals))
        : options (signalOptions s)
        <> [  withRequired SignalsSelectorIsMissing cSignals (signalSelector s)
            , withDefault  cOnlyIfMissing    (signalOnlyIfMissing s) ]
      
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
    removeSignalsPath      :: SignalsPath
  , removeSignalsOptions   :: Options
  } deriving Show

instance Default RemoveSignals where
  def                       = RemoveSignals {
    removeSignalsPath        = def
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

removeSignals :: RemoveSignals -> Builder
removeSignals r = format builders
  where
    builders =
       (Just    (withEvent EventRemoveSignals))
       : options (removeSignalsOptions r)
       <> [withRequired RemoveSignalsPathIsEmpty cPaths (removeSignalsPath r) ]

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
    executeScriptJS      :: Script
  , executeAttributes    :: Attributes
  , executeAutoRemove    :: AutoRemove
  , executeScriptOptions :: Options
  } deriving Show

instance Default ExecuteScript where
  def                    = ExecuteScript {
    executeScriptJS      = def
  , executeAttributes    = def
  , executeAutoRemove    = def
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
      (Just    (withEvent EventExecuteScript))
      : options (executeScriptOptions e)
      <> [withRequired ExecuteScriptIsMissing cScript (executeScriptJS e)]
      <> [withDefault cAttributes (executeAttributes e)]
      <> [withDefault cAutoRemove    (executeAutoRemove e) ]

      

