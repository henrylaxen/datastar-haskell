module ServerSentEventGenerator (

    EventType(..)
  , HttpVersion(..)
  , ToBuilder(..)
  , MergeMode(..)
  , Send(..)
  , MergeFragment
  , sseHeaders
  , send
  , sendFragments
  , mergeFragments
  
  ) where

import           Constants
import           Data.ByteString.Builder
import           Data.Default              ( Default(..) )
import ServerSentEventGenerator.Internal

-- $setup
-- >>> import           Data.Functor.Identity
-- >>> import           Data.Maybe
-- >>> import           Data.Text                 ( Text )
-- >>> import qualified Data.Text.Encoding        as T


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
  , sendEventId       :: Maybe Builder
  , sendRetryDuration :: Maybe Int
  } deriving Show

instance Default Send where
  def = Send EventMergeFragments [] Nothing Nothing


-- | convert a Send data type to a Builder, ready to be sent down the wire
--
-- Example
--
-- >>>  send $ Send EventMergeFragments ["line 1", "line 2"] (Just "alpah123") Nothing
-- "datastar-merge-fragments\nid: alpah123\ndata: line 1\ndata: line 2\n\n"
--
-- >>> send (def {sendDataLines = ["line1, line2"]})
-- "datastar-merge-fragments\ndata: line1, line2\n\n"

send :: Send -> Builder
send s = format builders <> "\n"
  where
    builders = 
        [Just (toBuilder (sendEventType s))]
        <> options (sendEventId s) (sendRetryDuration s)
      <> ( map (Just . ("data: " <>)) (sendDataLines s))

-- | A convenience function that takes a list of ByteString/String/Text and
--   outputs a Builder, assuming the rest of the Send Data Type fields are
--   the defaults.
--
-- Example
--
-- >>> sendFragments (["l1", "l2"] :: [String])
-- "datastar-merge-fragments\ndata: l1\ndata: l2\n\n"
--
-- >>> sendFragments (["l1", "l2"] :: [Text])
-- "datastar-merge-fragments\ndata: l1\ndata: l2\n\n"


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

sendFragments :: ToBuilder a => [a] -> Builder
sendFragments s = send def {sendDataLines = map toBuilder s}

data MergeFragment = MergeFragment {
     mergeData              :: [Builder]
   , mergeSelector          :: Maybe Builder    -- > selector: "abc123"
   , mergeMode              :: Maybe MergeMode  -- > 
   , mergesettleDuration    :: Maybe Int
   , mergeUseUiewTransition :: Maybe Bool
   , mergeEventId           :: Maybe Builder
   , mergeRetryDuration     :: Maybe Int
   } deriving Show

instance Default MergeFragment where
  def                    = MergeFragment {
       mergeData              = []
    ,  mergeSelector          = Nothing
    ,  mergeMode              = Just Morph
    ,  mergesettleDuration    = Just cDefaultSettleDurationMs
    ,  mergeUseUiewTransition = Just False
    ,  mergeEventId           = Nothing
    ,  mergeRetryDuration     = Just cDefaultSseRetryDurationMs }

mergeFragments :: MergeFragment -> Builder
mergeFragments m = format builders <> "\n"
  where
    builders = 
      [ Just (toBuilder EventMergeFragments) ]
      <> options (mergeEventId m) (mergeRetryDuration m)
      <> withDefaults
      <> ( map (Just . ("data: " <>)) (mergeData m))
    withDefaults :: [Maybe Builder]
    withDefaults = 
      [  maybeDefault  (mergeMode m)
      ,  maybeDefault  (mergeUseUiewTransition m)
      ,  maybeDefault  (mergeUseUiewTransition m) ]
  
