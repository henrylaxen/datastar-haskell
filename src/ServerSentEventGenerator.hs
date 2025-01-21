module ServerSentEventGenerator where

--     HttpVersion(..)
--   , Sender(..)
--   , ToBuilder(..)
--   , Options(..)
--   , FragmentOptions(..)
--   , EventType(..)
--   , MergeMode(..)
--   , Send(..)
--   , MergeFragments(..)
--   , RemoveFragment(..)
--   , MergeSignals(..)
--   , RemoveSignals(..)
--   , ExecuteScript(..)
--   , sseHeaders
--   , sendPure
--   , fragments
--   , mergeFragments
--   , removeFragment
--   , mergeSignals
--   , removeSignals
--   , executeScript
--   , withOptions
--   , sampleDataLines
--   , sp


import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Types
import Data.ByteString.Builder --  ( Builder )
import Data.Default ( Default(..) )
import Data.Maybe
import System.IO
import Control.Exception

-- import ServerSentEventGenerator.Internal
-- import ServerSentEventGenerator.Newtypes

-- $setup
-- >>> import           Data.Functor.Identity
-- >>> import           Data.Maybe
-- >>> import           Data.Text                 ( Text )
-- >>> import qualified Data.Text.Encoding        as T

-- Bug in sse.py, the event_id is an Int

sampleDataLines :: [Builder]
sampleDataLines = ["line 1", "line 2"]
-- sp :: Send -> IO ()
-- sp = send . sendPure


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


-- | All server sent events can contain and Event Id and a Retry Duration as an option
--   This works, because if the options are equal to their defaults, they will
--   be removed from the output

sendPure :: (ToBuilder a) => EventType -> [a] -> Options -> Builder
sendPure eventType dataLines options = buildLines (a:b:c)
  where
    withSSEdefault value defaultValue field = if value ==  defaultValue then mempty
      else field <> ": " <> toBuilder value
    a = "event: " <> toBuilder eventType
    b = toBuilder options
    c = map (\x -> cData <> ": " <> toBuilder x) dataLines

-- t1 = sp (send MergeFragments sampleDataLines (Options "abc123" 100))
-- t2 = sp (send MergeFragments sampleDataLines def)

-- sendSSE :: SSE -> [Builder]
-- sendSSE (SSE a b c) = send a b c

-- data MergeFragments = MergeFragments {
--      mergeData              :: DataLines
--    , mergeSelector          :: Selector
--    , mergeMode              :: MergeMode  -- > Morph is default
--    , mergeFragmentOptions   :: FragmentOptions
--    , mergeOptions           :: Options
--    } deriving Show

-- instance Default MergeFragments where
--   def                        = MergeFragments {
--        mergeData              = def
--     ,  mergeSelector          = def
--     ,  mergeMode              = def
--     ,  mergeFragmentOptions   = def
--     ,  mergeOptions           = def }

-- data FragmentOptions = FragmentOptions {
--     settleDuration    :: SettleDuration
--   , useViewTransition :: UseViewTransition
--   } deriving (Show)

-- -- | the MergeFragments and RemoveFragment data types share these options

-- instance Default FragmentOptions where
--   def = FragmentOptions {
--     settleDuration     = def
--   , useViewTransition  = def
--   }


-- mergeFragment fragments mergeMode mergeSelector settleDuration useViewTransition
--   where
--     a = withEvent Edata MergeFragments
--     b = withDefault cDefaultMergeMode
--     c = withDefault cRetryDuration cDefaultSseRetryDurationMs (retryDuration options)
--     d = map withData dataLines
    

-- mergeFragment =
--   where
--     e = withEvent (toBuilder MergeFragments)
    

-- mergeFragments :: MergeFragments -> Builder
-- mergeFragments m = format builders
--   where
    
--     builders =
--       [ Just (withEvent EventMergeFragments) ]
--       <> options (mergeOptions m)
--       <> withDefaults
--       <> withBuilderList (mergeData  m)
--     withDefaults =
--       [  withDefault        (mergeMode m)
--       ,  withDefault        (mergeSelector m)
--       ] <> fragmentOptions  (mergeFragmentOptions m)

-- withDefault dStarEvent defaultValue value 

mergeFragments :: (ToBuilder a) => [a] -> Selector -> MergeMode -> FragmentOptions -> Options -> Builder
mergeFragments fragments selector mode fragOptions options = buildLines (a:b:c:d:e:f)
  where
    a = "event: " <> toBuilder MergeFragments
    b = toBuilder options
    c = toBuilder selector
    d = withDefault cMerge cDefaultMergeMode (toBuilder mode)
    e = toBuilder fragOptions
    f = withFragments fragments

-- t1 = mergeFragments sampleDataLines def def def def
-- t2 = mergeFragments sampleDataLines (SEL "#id") def def def
-- t3 = mergeFragments sampleDataLines (SEL "#id") Inner def def
-- t4 = mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 False) def
-- t5 = mergeFragments sampleDataLines (SEL "#id") Inner (FO 1 True) (O "abc123" 10)
-- t6 = sp [t1,t2,t3,t4,t5]

-- Bug in sse.py, the selector is made optional

removeFragments :: Selector -> FragmentOptions -> Options -> Builder
removeFragments selector fragOptions options = buildLines [a,b,c,d]
  where
    a = "event: " <> toBuilder RemoveFragments
    b = toBuilder options
    c = if selector == def
          then bug RemoveFragmentSelectorIsMissing
          else toBuilder selector
    d = toBuilder fragOptions
      
-- t1 = sp [removeFragments def def def] `catch`
--         (\(e :: ServerSentEventGeneratorExceptions) -> print e)
-- t2 = removeFragments (SEL "#id") def def 
-- t3 = removeFragments (SEL "#id") (FO 1 False) def
-- t4 = removeFragments (SEL "#id") (FO 1 True) def
-- t5 = removeFragments (SEL "#id") (FO 1 False) (O "abc123" 10)
-- t6 = t1 >> sp [t2,t3,t4,t5]




