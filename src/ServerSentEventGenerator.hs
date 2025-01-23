module ServerSentEventGenerator  (

    HttpVersion(..)
  , ToBuilder(..)
  , Options(..)
  , FragmentOptions(..)
  , EventType(..)
  , MergeMode(..)
  , mergeFragments
  , removeFragments
  , mergeSignals
  , removeSignals
  , executeScript
  , sseHeaders
  , sendPure
  , send

  -- $setup

  ) where

import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Types
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )
import Control.Monad.IO.Class
-- import Data.Text ( Text )
import NeatInterpolation
-- import Data.ByteString.Lazy.UTF8
-- import           Data.Functor.Identity
-- import Control.Exception
-- import Control.Concurrent.MVar
-- import ServerSentEventGenerator.Internal
-- import ServerSentEventGenerator.Newtypes



-- $setup
-- >>> import           Data.Functor.Identity
-- >>> import           Data.Maybe
-- >>> import           Data.Text                 ( Text )
-- >>> import qualified Data.Text.Encoding        as T
-- >>> import Data.ByteString.Lazy ( ByteString )
-- >>> import Control.Exception




-- <<Bug>> in sse.py, the event_id is an Int


-- | returns the Http header for an SSE depending
--   on the Http version you are using. Note: you will
--   have to implement an instance of the HttpVersion class
--   for whichever web server you are using
--
-- Example:
--
-- >>> import           Data.Functor.Identity
-- >>> runIdentity $ sseHeaders
-- "Cache-control: no-cache\nContent-type: text/event-stream\nConnection: keep-alive\n"

sseHeaders :: HttpVersion m => m Builder
sseHeaders = do
  b <- isHttpVersion1_1
  return $ if b then sseHeaders1_1 else sseHeaders2
  where
    sseHeaders2 = "Cache-control: no-cache\nContent-type: text/event-stream\n"
    sseHeaders1_1 = sseHeaders2 <> "Connection: keep-alive\n"

send :: (MonadIO m, ToBuilder a) => EventType -> [a] -> Options -> m ()
send a b c = sendM (sendPure a b c)

-- | All server sent events can contain and Event Id and a Retry Duration as an option
--   This works, because if the options are equal to their defaults, they will
--   be removed from the output

sendPure :: (ToBuilder a) => EventType -> [a] -> Options -> Builder
sendPure eventType dataLines options = mconcat (buildLines (a:b:c)) <> "\n\n"
  where
--     withSSEdefault value defaultValue field = if value == defaultValue then mempty
--       else field <> ": " <> toBuilder value
    a = "event: " <> toBuilder eventType
    b = toBuilder options
    c = map (\x -> cData <> ": " <> toBuilder x) dataLines

{- | >>> :{
do
  let
    sampleDataLines :: [Builder]
    sampleDataLines = ["line 1", "line 2"]
    them = [
        mergeFragments sampleDataLines noSelector def def def
      , mergeFragments sampleDataLines (SEL "#id") def def def
      , mergeFragments sampleDataLines (SEL "#id") Inner def def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10) ]
  test them
:}
event: datastar-merge-fragments
data: data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: data: selector #id
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: data: selector #id
data: mergeMode inner
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: data: selector #id
data: mergeMode inner
data: settleDuration 1
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
id: abc123
retry: 10
data: data: selector #id
data: mergeMode inner
data: settleDuration 1
data: useViewTransition true
data: fragments line 1
data: fragments line 2
<BLANKLINE>
-}

mergeFragments :: (ToBuilder a) => [a] -> Selector a -> MergeMode -> FragmentOptions -> Options -> Builder
mergeFragments fragments selector mode fragOptions =  sendPure MergeFragments (buildLines (a:b:c:d))
  where
    a = toBuilder selector
    b = withDefault cMerge cDefaultMergeMode (toBuilder mode)
    c = toBuilder fragOptions
    d = withList cFragments fragments

{- | >>> :{
-- import Data.Text       ( Text )
-- import Data.ByteString.Lazy ( ByteString )
-- import Control.Exception
do
  let
    rt1 :: IO ()
    rt2,rt3,rt4,rt5 :: Builder
    rt1 = test [removeFragments noSelector def def] `catch`
             (\(e :: ServerSentEventGeneratorExceptions) -> print e)
    rt2 = removeFragments (SEL ("#id" :: Builder)) def def
    rt3 = removeFragments (SEL ("#id" :: Text)) (FO 1 False) def
    rt4 = removeFragments (SEL ("#id" :: String)) (FO 1 True) def
    rt5 = removeFragments (SEL ("#id" :: ByteString)) (FO 1 False) (O "abc123" 10)
  rt1 >> test [rt2,rt3,rt4,rt5]
:}
<interactive>: The selector field is required in RemoveFragment
event: datastar-remove-fragments
data: data: selector #id
<BLANKLINE>
event: datastar-remove-fragments
data: data: selector #id
data: settleDuration 1
<BLANKLINE>
event: datastar-remove-fragments
data: data: selector #id
data: settleDuration 1
data: useViewTransition true
<BLANKLINE>
event: datastar-remove-fragments
id: abc123
retry: 10
data: data: selector #id
data: settleDuration 1
<BLANKLINE>
-}

-- <<Bug>> in sse.py, the selector is made optional
removeFragments :: (ToBuilder a) => Selector a -> FragmentOptions -> Options -> Builder
removeFragments selector fragOptions = sendPure RemoveFragments (buildLines [a,b])
  where
    s = toBuilder selector
    a = if s == def then bug RemoveFragmentSelectorIsMissing else s
    b = toBuilder fragOptions

-- <<Bug>> in sse.py or README.md,
-- sse.py has signals as an array, README.md has signals as a string
-- I think it would be better if it were an array. That would also make
-- an empty list a valid mergeSignals request, which might be more
-- convenient for programmers.  Of course it's up to you.
-- if array -> mergeSignals :: (ToBuilder a) => [a] -> Bool -> Options -> Builder
mergeSignals :: (ToBuilder a) => a -> Bool -> Options -> Builder
mergeSignals signals onlyIfMissing = sendPure MergeSignals (buildLines [a,b])
  where
    a = if (toBuilder signals) == mempty
          then bug SignalsSelectorIsMissing
          else withDefault cSignals nil (toBuilder signals)
-- if array -> else withList cSignals signals
    b = withDefault cOnlyIfMissing cDefaultOnlyIfMissing onlyIfMissing

{- | >>> :{
-- import Data.Text ( Text )
-- import Control.Exception
do
  let
    testMergeSignal :: Text
    testMergeSignal = "{\"a\":\"b\",\"c\":true,\"d\":1}"
    mst1 = test [mergeSignals nil def def] `catch`
            (\(e :: ServerSentEventGeneratorExceptions) -> print e)
    them = [
        mergeSignals  testMergeSignal False def
     ,  mergeSignals  testMergeSignal True (O "abc123" 10) ]
  mst1 >> test them
:}
<interactive>: The selector field is required in MergeSignals
event: datastar-merge-signals
data: data: signals {"a":"b","c":true,"d":1}
<BLANKLINE>
event: datastar-merge-signals
id: abc123
retry: 10
data: data: signals {"a":"b","c":true,"d":1}
data: onlyIfMissing true
<BLANKLINE>
-}

-- <<bug>> Maybe? sse.py allows the paths to be empty,
--                README.md does not specify
removeSignals :: (ToBuilder a) => [a] -> Options -> Builder
removeSignals paths = sendPure RemoveSignals (buildLines c)
  where
    c = withList cRemoveSignals paths

{- | >>> :{
do
  let
    testRemoveSignal = ["velocity.x", "velocity.y", "position"] :: [Builder]
    them = [
        removeSignals nils def
      , removeSignals  testRemoveSignal def
      , removeSignals  testRemoveSignal (O "abc123" 10) ]
  test them
:}
event: datastar-remove-signals
<BLANKLINE>
event: datastar-remove-signals
data: data: datastar-remove-signals velocity.x
data: datastar-remove-signals velocity.y
data: datastar-remove-signals position
<BLANKLINE>
event: datastar-remove-signals
id: abc123
retry: 10
data: data: datastar-remove-signals velocity.x
data: datastar-remove-signals velocity.y
data: datastar-remove-signals position
<BLANKLINE>
-}

-- <<bug>> Maybe? sse.py allows the script to be empty, and type is array
--                README.md does not specify, and type is string
executeScript :: (ToBuilder a, Eq b, Monoid b, ToBuilder b) =>
                 [a] -> [b] -> Bool -> Options -> Builder
executeScript script attributes autoRemove = sendPure ExecuteScript (buildLines (a <> b <> [c]))
  where
    a = withList cExecuteScript script
    b = if null attributes
          then [cData <> ": " <> cAttributes <> " " <> cDefaultAttributes]
          else withList cAttributes attributes
    c = withDefault cAutoRemove cDefaultAutoRemove autoRemove

{- | >>> :{
do
  let
    testScript     = ["window.location = \"https://data-star.dev\""] :: [Builder]
    testAttributes = ["type text/javascript"] :: [Builder]
    them = [
        executeScript nils nils True def
      , executeScript  testScript nils False def
      , executeScript  testScript testAttributes False def
      , executeScript  testScript testAttributes True (O "abc123" 10)  ]
  test them
:}
event: datastar-execute-script
data: data: attributes type module
<BLANKLINE>
event: datastar-execute-script
data: data: datastar-execute-script window.location = "https://data-star.dev"
data: attributes type module
data: autoRemove false
<BLANKLINE>
event: datastar-execute-script
data: data: datastar-execute-script window.location = "https://data-star.dev"
data: attributes type text/javascript
data: autoRemove false
<BLANKLINE>
event: datastar-execute-script
id: abc123
retry: 10
data: data: datastar-execute-script window.location = "https://data-star.dev"
data: attributes type text/javascript
<BLANKLINE>
-}

