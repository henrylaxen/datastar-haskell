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
  ) where

import Data.Text ( Text )
import Data.ByteString.Lazy.UTF8
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Types
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )
import Control.Exception
import NeatInterpolation
-- import Control.Concurrent.MVar
import Control.Monad.IO.Class
import           Data.Functor.Identity

-- import ServerSentEventGenerator.Internal
-- import ServerSentEventGenerator.Newtypes

-- $setup
{- | >>> :{
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Text                 ( Text )
import qualified Data.Text.Encoding        as T
:}
-}


-- <<Bug>> in sse.py, the event_id is an Int

sampleDataLines :: [Builder]
sampleDataLines = ["line 1", "line 2"]

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

send :: (MonadIO m, ToBuilder a) => EventType -> [a] -> Options -> m ()
send a b c = sendM (sendPure a b c)

-- | All server sent events can contain and Event Id and a Retry Duration as an option
--   This works, because if the options are equal to their defaults, they will
--   be removed from the output

sendPure :: (ToBuilder a) => EventType -> [a] -> Options -> Builder
sendPure eventType dataLines options = (buildLines (a:b:c)) <> "\n\n"
  where
--     withSSEdefault value defaultValue field = if value == defaultValue then mempty
--       else field <> ": " <> toBuilder value
    a = "event: " <> toBuilder eventType
    b = toBuilder options
    c = map (\x -> cData <> ": " <> toBuilder x) dataLines

{- | >>> :{
do
  let
    them = [
        mergeFragments sampleDataLines noSelector def def def
      , mergeFragments sampleDataLines (SEL "#id") def def def
      , mergeFragments sampleDataLines (SEL "#id") Inner def def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10)
     ]
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

{- |  :{
test :: [Builder] -> IO ()
test = mapM_ sendM
ts :: [Builder]
ts =
  let
    mt1,mt2,mt3,mt4,mt5 :: Builder
    mt1 = mergeFragments sampleDataLines noSelector def def def
    mt2 = mergeFragments sampleDataLines (SEL "#id") def def def
    mt3 = mergeFragments sampleDataLines (SEL "#id") Inner def def
    mt4 = mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def
    mt5 = mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10)
  in [mt1,mt2,mt3,mt4,mt5]
ts
:}
abc
-}


{- |  :{
do
  let
    them = [
      sendM (mergeFragments sampleDataLines noSelector def def def)
    , sendM (mergeFragments sampleDataLines (SEL "#id") def def def)
    , sendM (mergeFragments sampleDataLines (SEL "#id") Inner def def)
    , sendM (mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def)
    , sendM (mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10))
    ]
  test them
:}
-}

mergeFragments :: (ToBuilder a) => [a] -> Selector a -> MergeMode -> FragmentOptions -> Options -> Builder
mergeFragments fragments selector mode fragOptions =  sendPure MergeFragments [buildLines (a:b:c:d)]
  where
    a = toBuilder selector
    b = withDefault cMerge cDefaultMergeMode (toBuilder mode)
    c = toBuilder fragOptions
    d = withList cFragments fragments


-- mt1,mt2,mt3,mt4,mt5 :: Builder
-- mt :: IO ()
-- mt1 = mergeFragments sampleDataLines noSelector def def def
-- mt2 = mergeFragments sampleDataLines (SEL "#id") def def def
-- mt3 = mergeFragments sampleDataLines (SEL "#id") Inner def def
-- mt4 = mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def
-- mt5 = mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10)
-- mt = test [mt1,mt2,mt3,mt4,mt5]

-- <<Bug>> in sse.py, the selector is made optional  
removeFragments :: (ToBuilder a) => Selector a -> FragmentOptions -> Options -> Builder
removeFragments selector fragOptions = sendPure RemoveFragments [buildLines [a,b]]
  where
    s = toBuilder selector
    a = if s == def then bug RemoveFragmentSelectorIsMissing else s
    b = toBuilder fragOptions

-- rt2,rt3,rt4,rt5 :: Builder
-- rt, rt1 :: IO ()
-- rt1 = test [removeFragments noSelector def def] `catch`
--         (\(e :: ServerSentEventGeneratorExceptions) -> print e)
-- rt2 = removeFragments (SEL ("#id" :: Builder)) def def
-- rt3 = removeFragments (SEL ("#id" :: Text)) (FO 1 False) def
-- rt4 = removeFragments (SEL ("#id" :: String)) (FO 1 True) def
-- rt5 = removeFragments (SEL ("#id" :: ByteString)) (FO 1 False) (O "abc123" 10)
-- rt = rt1 >> test [rt2,rt3,rt4,rt5]


-- <<Bug>> in sse.py or README.md,
-- sse.py has signals as an array, README.md has signals as a string
-- I think it would be better if it were an array. That would also make
-- an empty list a valid mergeSignals request, which might be more
-- convenient for programmers.  Of course it's up to you.
-- if array -> mergeSignals :: (ToBuilder a) => [a] -> Bool -> Options -> Builder
mergeSignals :: (ToBuilder a) => a -> Bool -> Options -> Builder
mergeSignals signals onlyIfMissing = sendPure MergeSignals [buildLines [a,b]]
  where
    a = if (toBuilder signals) == mempty
          then bug SignalsSelectorIsMissing
          else withDefault cSignals nil (toBuilder signals)
-- if array -> else withList cSignals signals
    b = withDefault cOnlyIfMissing cDefaultOnlyIfMissing onlyIfMissing

-- testMergeSignal :: Text
-- testMergeSignal = [trimming|{"a":"b","c":true,"d":1}|]
-- mst2,mst3 :: Builder
-- mst, mst1 :: IO ()
-- mst1 = test [mergeSignals nil def def] `catch`
--         (\(e :: ServerSentEventGeneratorExceptions) -> print e)
-- mst2 = mergeSignals  testMergeSignal False def
-- mst3 = mergeSignals  testMergeSignal True (O "abc123" 10)
-- mst = mst1 >> test [mst2,mst3]

-- <<bug>> Maybe? sse.py allows the paths to be empty,
--                README.md does not specify
removeSignals :: (ToBuilder a) => [a] -> Options -> Builder
removeSignals paths = sendPure RemoveSignals [buildLines c]
  where
    c = withList cRemoveSignals paths
-- testRemoveSignal = ["velocity.x", "velocity.y", "position"] :: [Builder]

-- rst1,rst2,rst3 :: Builder
-- rst :: IO ()
-- rst1 = removeSignals (mempty :: [Builder]) def
-- rst2 = removeSignals  testRemoveSignal def
-- rst3 = removeSignals  testRemoveSignal (O "abc123" 10)
-- rst = test [rst1,rst2,rst3]

-- <<bug>> Maybe? sse.py allows the script to be empty, and type is array
--                README.md does not specify, and type is string
executeScript :: (ToBuilder a, Eq b, Monoid b, ToBuilder b) =>
                 [a] -> [b] -> Bool -> Options -> Builder
executeScript script attributes autoRemove = sendPure ExecuteScript [buildLines (a <> b <> [c])  ]
  where
    a = withList cExecuteScript script
    b = if null attributes
          then [cData <> ": " <> cAttributes <> " " <> cDefaultAttributes]
          else withList cAttributes attributes
    c = withDefault cAutoRemove cDefaultAutoRemove autoRemove
-- testScript     = [[trimming|window.location = "https://data-star.dev"|]]
-- testAttributes = [[trimming|type text/javascript|]]
-- noList = [] :: [Builder]

-- est1,est2,est3,est4 :: Builder
-- est :: IO ()
-- est1 = executeScript noList noList True def
-- est2 = executeScript  testScript noList False def
-- est3 = executeScript  testScript testAttributes False def
-- est4 = executeScript  testScript testAttributes True (O "abc123" 10)
-- est  = test [est1,est2,est3,est4]
