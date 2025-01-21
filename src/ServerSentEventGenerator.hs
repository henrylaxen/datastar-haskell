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


-- import ServerSentEventGenerator.Internal
-- import ServerSentEventGenerator.Newtypes

-- $setup
-- >>> import           Data.Functor.Identity
-- >>> import           Data.Maybe
-- >>> import           Data.Text                 ( Text )
-- >>> import qualified Data.Text.Encoding        as T

sp = hPutBuilder stdout 

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
--   This works, because of the options in opt are equal to their defaults, they will
--   later be removed from the output

-- send :: ToBuilder a => DatastarEventType -> [a] -> Options -> [Builder]
-- send datastarEventType dataLines options = (catMaybes (a:b:c:d)) <> ["\n"]
--   where
--     a = withEvent Eevent datastarEventType
--     b = withDefault cEventId mempty (eventId options)
--     c = withDefault cRetryDuration cDefaultSseRetryDurationMs (retryDuration options)
--     d = map withData dataLines

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

-- merge :: Builder -> MergeMode -> FragmentOptions -> Options -> Builder
-- merge selector mode fragOptions options =


