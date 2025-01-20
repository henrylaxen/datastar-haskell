module ServerSentEventGenerator (

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
  ) where

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

sp = hPutBuilder stdout . mconcat

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

-- options ::  Options -> [Maybe Builder]
-- options opt =
--   [ withDefault (withColon cEventId)        (eventId opt)
--   , withDefault (withColon cRetryDuration)  (toBuilder (retryDuration opt)) ]


send :: ToBuilder a => EventType -> [a] -> Options -> [Builder]
send eventType dataLines options = catMaybes (e:i:r:d)
  where
    e = withEvent (toBuilder eventType)
    d = map withData dataLines
    i = withDefault cEventId mempty (eventId options)
    r = withDefault cRetryDuration cDefaultSseRetryDurationMs (retryDuration options)

t1 = sp (send MergeFragments sampleDataLines (Options "abc123" 100))
t2 = sp (send MergeFragments sampleDataLines def)

data SSE = SSE {
    sEventType     :: EventType
  , sDataLines     :: [Builder]
  , sOptions       :: Options
  } deriving Show

{- From the README.MD
ServerSentEventGenerator.send(
    eventType: EventType,
    dataLines: string[],
    options?: {
        eventId?: string,
        retryDuration?: durationInMilliseconds
    }) -}


-- | All server sent events can contain and Event Id and a Retry Duration as an option
--   This works, because of the options in opt are equal to their defaults, they will
--   later be removed from the output
