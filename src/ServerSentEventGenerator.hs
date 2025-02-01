module ServerSentEventGenerator  
{-
    HttpVersion(..)
  , ToText(..)
  , Options(..)
  , FragmentOptions(..)
  , EventType(..)
  , MergeMode(..)
--   , SSE
  , SSEstream
  , SSEapp(..)
  , Selector(..)
  , mergeFragments
  , removeFragments
  , mergeSignals
  , removeSignals
  , executeScript
  , singleThreaded
  , sseHeaders
  , sendPure
--   , sendM
  , test
  , toPre
  -- $setup
  ) -}

where

import Data.ByteString.Builder ( Builder )
import Data.Default ( Default(..) )
import Data.Text ( Text )
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Types
-- import qualified Data.Text as T ( lines )
-- import Data.String

-- $setup
-- >>> import Data.Functor.Identity
-- >>> import Data.Maybe
-- >>> import Control.Exception


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
    sseHeaders2 = "HTTP/1.1 200 OK\nCache-control: no-cache\nContent-type: text/event-stream\n"
    sseHeaders1_1 = sseHeaders2 <> "Connection: keep-alive\n"

-- | Send is supposed to send a unit of text to the client.  Unfortunately, I
--   don't know how to do this in a server independent way. 

-- send :: Text -> IO ()
-- send = error "not implemented, please see sseSend in SnamSSE for ideas"

-- | All server sent events can contain and Event Id and a Retry Duration as an option
--   This works, because if the options are equal to their defaults, they will
--   be removed from the output

sendPure :: EventType -> [Text] -> Options -> Text
sendPure eventType dataLines options = mconcat (buildLines (a:b:dataLines)) <> "\n\n"
  where
    a = cEvent <> cSColon <> prompt eventType
    b = prompt options
{- | >>> :{
do
  let
    sampleDataLines :: [Text]
    sampleDataLines = ["line 1", "line 2"]
    them = [
        mergeFragments sampleDataLines def def def def
      , mergeFragments sampleDataLines (SEL "#id") def def def
      , mergeFragments sampleDataLines (SEL "#id") Inner def def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def
      , mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10) ]
  test them
:}
event: datastar-merge-fragments
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: selector #id
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: selector #id
data: mergeMode inner
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
data: selector #id
data: mergeMode inner
data: settleDuration 1
data: fragments line 1
data: fragments line 2
<BLANKLINE>
event: datastar-merge-fragments
id: abc123
retry: 10
data: selector #id
data: mergeMode inner
data: settleDuration 1
data: useViewTransition true
data: fragments line 1
data: fragments line 2
<BLANKLINE>
-}

-- | Insert "data: " and the given text in front of each element of the list
-- | >>> withList "fragments" ["l1","l2"]
--   ["data: fragments l1","data: fragments l2"]
-- mergeFragments
--   :: (ToText p1, ToText p2, ToText p3, IsString a) =>
--      [a] -> p3 -> p2 -> p1 -> Options -> Text
-- mergeFragments :: StringLike a => [Text] -> Selector -> MergeMode -> FragmentOptions -> Options a -> Text
mergeFragments  :: Text -> Selector -> MergeMode -> FragmentOptions -> Options -> Text
mergeFragments fragments selector mode fragOptions =  sendPure MergeFragments (buildLines (a:b:c:d))
  where
    a = prompt selector
    b = withDefault cMerge cDefaultMergeMode (prompt mode)
    c = prompt fragOptions
    d = withList cFragments fragments

{- | >>> :{
do
  let
    rt1 :: IO ()
    rt2,rt3,rt4,rt5 :: Text
    rt1 = test [removeFragments def def def] `catch`
             (\(e :: ServerSentEventGeneratorExceptions) -> print e)
    rt2 = removeFragments (SEL "#id") def def
    rt3 = removeFragments (SEL "#id") (FO 1 False) def
    rt4 = removeFragments (SEL "#id") (FO 1 True) def
    rt5 = removeFragments (SEL "#id") (FO 1 False) (O "abc123" 10)
  rt1 >> test [rt2,rt3,rt4,rt5]
:}
The selector field is required in RemoveFragment
event: datastar-remove-fragments
data: selector #id
<BLANKLINE>
event: datastar-remove-fragments
data: selector #id
data: settleDuration 1
<BLANKLINE>
event: datastar-remove-fragments
data: selector #id
data: settleDuration 1
data: useViewTransition true
<BLANKLINE>
event: datastar-remove-fragments
id: abc123
retry: 10
data: selector #id
data: settleDuration 1
<BLANKLINE>
-}

removeFragments :: Selector  -> FragmentOptions -> Options -> Text
removeFragments selector fragOptions = sendPure RemoveFragments (buildLines [a,b])
  where
    s = prompt selector
    a = if s == def then bug RemoveFragmentSelectorIsMissing else s
    b = prompt fragOptions

{- | >>> :{
do
  let
    testMergeSignal :: Text
    testMergeSignal = "{\"a\":\"b\",\"c\":true,\"d\":1}"
    mst1 = test [mergeSignals def def def] `catch`
            (\(e :: ServerSentEventGeneratorExceptions) -> print e)
    them = [
        mergeSignals  testMergeSignal False def
     ,  mergeSignals  testMergeSignal True (O "abc123" 10) ]
  mst1 >> test them
:}
The selector field is required in MergeSignals
event: datastar-merge-signals
data: signals {"a":"b","c":true,"d":1}
<BLANKLINE>
event: datastar-merge-signals
id: abc123
retry: 10
data: signals {"a":"b","c":true,"d":1}
data: onlyIfMissing true
<BLANKLINE>
-}

mergeSignals ::  Text -> Bool -> Options -> Text
mergeSignals signals onlyIfMissing = sendPure MergeSignals (buildLines [a,b])
  where
    a = if signals == mempty
          then bug SignalsSelectorIsMissing
          else withDefault cSignals "" signals
    b = withDefault cOnlyIfMissing (prompt cDefaultOnlyIfMissing) (prompt onlyIfMissing)

{- | >>> :{
do
  let
    testRemoveSignal = ["velocity.x", "velocity.y", "position"] :: [Text]
    them = [
        removeSignals [] def
      , removeSignals  testRemoveSignal def
      , removeSignals  testRemoveSignal (O "abc123" 10) ]
  test them
:}
event: datastar-remove-signals
<BLANKLINE>
event: datastar-remove-signals
data: datastar-remove-signals velocity.x
data: datastar-remove-signals velocity.y
data: datastar-remove-signals position
<BLANKLINE>
event: datastar-remove-signals
id: abc123
retry: 10
data: datastar-remove-signals velocity.x
data: datastar-remove-signals velocity.y
data: datastar-remove-signals position
<BLANKLINE>
-}

removeSignals :: Text -> Options -> Text
removeSignals paths = sendPure RemoveSignals (buildLines c)
  where
    c = withList cRemoveSignals paths

{- | >>> :{
do
  let
    testScript     = ["window.location = \"https://data-star.dev\""] :: [Text]
    testAttributes = ["type text/javascript"] :: [Text]
    them = [
        executeScript [] [] True def
      , executeScript  testScript [] False def
      , executeScript  testScript testAttributes False def
      , executeScript  testScript testAttributes True (O "abc123" 10)  ]
  test them
:}
event: datastar-execute-script
data: attributes type module
<BLANKLINE>
event: datastar-execute-script
data: datastar-execute-script window.location = "https://data-star.dev"
data: attributes type module
data: autoRemove false
<BLANKLINE>
event: datastar-execute-script
data: datastar-execute-script window.location = "https://data-star.dev"
data: attributes type text/javascript
data: autoRemove false
<BLANKLINE>
event: datastar-execute-script
id: abc123
retry: 10
data: datastar-execute-script window.location = "https://data-star.dev"
data: attributes type text/javascript
<BLANKLINE>
-}

executeScript :: Text -> Text -> Bool -> Options -> Text
executeScript script attributes autoRemove = sendPure ExecuteScript (buildLines (a : b <> [c]))
  where
    a = prefixed cExecuteScript script 
    b = if attributes == ""
          then [cData <> ": " <> cAttributes <> " " <> cDefaultAttributes]
          else withList cAttributes attributes
    c = withDefault cAutoRemove (prompt cDefaultAutoRemove) (prompt autoRemove)



