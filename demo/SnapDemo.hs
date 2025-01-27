{-# OPTIONS_GHC -fno-warn-orphans #-}
module SnapDemo where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy
import           Data.ByteString.Builder
import           Snap hiding ( headers, HttpVersion )
import qualified System.IO.Streams       as Streams
import Data.ByteString.Builder.Extra
-- import           Data.Text.Encoding
-- import           NeatInterpolation   hiding (text)
import           Codec.Binary.UTF8.String
import           ServerSentEventGenerator.Class
import           ServerSentEventGenerator.Types


type Tickle = (Int -> Int) -> IO ()

sseRun :: MonadSnap m => SSEapp -> m ()
sseRun (SSEapp app) = do
  -- request <- Snap.getRequest
  Snap.escapeHttp $ \tickle _ writeEnd -> do
    pingThreadId <-forkIO (ping tickle writeEnd)
    print ("after headers" :: Builder)
    app writeEnd
    print ("after app" :: Builder)
    killThread pingThreadId
    Streams.write Nothing writeEnd    

sseWrite :: Builder-> SSEstream -> IO ()
sseWrite x writeEnd = do
--   let s = Data.Text.unpack .decodeUtf8 . toLazyByteString $ x
  let s = decode . unpack . toLazyByteString $ x
  putStrLn ("sseWrite: " <> s)
  Streams.write (Just x) writeEnd
  Streams.write (Just flush) writeEnd

ping :: Tickle -> SSEstream -> IO ()  
ping tickle writeEnd = forever $ do
  print ("ping" :: Builder)
  (Streams.write  (Just ":\n\n")) writeEnd
    `catch` (\(e :: SomeException) -> print e >> return ())
    -- as near as I can tell, this never triggers
  tickle (max 60)
  threadDelay (11 * 1000 * 1000)

instance HttpVersion Snap  where
  isHttpVersion1_1 = do
    version <- rqVersion <$>getRequest
    return (version == (1,1))

-- headers :: Builder
-- headers = encodeUtf8Builder [trimming|
-- Cache-control: no-cache
-- Content-type: text/event-stream
-- Connection: keep-alive
-- |] <> "\n"



-- ping :: Tickle -> SSEstream -> ThreadId -> IO ()
-- ping = undefined

-- type EscapeHttpHandler
--  = ((Int -> Int) -> IO ())	

-- timeout modifier
-- -> InputStream ByteString	

-- socket read end
-- -> OutputStream Builder	

-- socket write end
-- -> IO ()

-- sseOpen :: MonadSnap m => (SSEstream -> IO ()) -> m ()
-- sseOpen f = do
--   escapeHttp $ \tickle _ writeEnd -> do
--     tickle (max (60*60))
--     Streams.write (Just (lazyByteString sseHeaders)) writeEnd
--     f writeEnd
-- sseWrite :: MonadIO m => SSEstream -> DsString -> m ()
-- sseWrite writeEnd dsStr = liftIO $ do
--   Streams.write (Just (lazyByteString dsStr)) writeEnd
--   Streams.write (Just flush) writeEnd
-- sseClose :: MonadIO m => SSEstream -> m ()
-- sseClose writeEnd = liftIO $ Streams.write Nothing writeEnd



-- | Start a ping thread in the background
-- forkPingThread :: ((Int -> Int) -> IO ()) -> IORef Bool -> SSEstream -> IO ()
-- forkPingThread tickle done conn = do
--     _ <- forkIO pingThread
--     return ()
--   where
--     pingThread = handle ignore $
--         let loop = do
--                 d <- readIORef done
--                 unless d $ do
--                     WS.sendPing conn (BC.pack "ping")
--                     tickle (max 60)
--                     threadDelay $ 10 * 1000 * 1000
--                     loop in
--         loop

--     ignore :: SomeException -> IO ()
--     ignore _   = return ()

--------------------------------------------------------------------------------
-- | This datatype allows you to set options for 'acceptRequestWith'.  It is
-- strongly recommended to use 'defaultAcceptRequest' and then modify the
-- various fields, that way new fields introduced in the library do not break
-- your code.
-- data AcceptRequest = AcceptRequest
--     { acceptSubprotocol :: !(Maybe B.ByteString)
--     -- ^ The subprotocol to speak with the client.  If 'pendingSubprotcols' is
--     -- non-empty, 'acceptSubprotocol' must be one of the subprotocols from the
--     -- list.
--     , acceptHeaders     :: !Headers
--     -- ^ Extra headers to send with the response.
--     }


--------------------------------------------------------------------------------
-- defaultAcceptRequest :: AcceptRequest
-- defaultAcceptRequest = AcceptRequest Nothing []


-- data PendingConnection = PendingConnection
--     { pendingOptions  :: !ConnectionOptions
--     -- ^ Options, passed as-is to the 'Connection'
--     , pendingRequest  :: !RequestHead
--     -- ^ Useful for e.g. inspecting the request path.
--     , pendingOnAccept :: !(Connection -> IO ())
--     -- ^ One-shot callback fired when a connection is accepted, i.e., *after*
--     -- the accepting response is sent to the client.
--     , pendingStream   :: !Stream
--     -- ^ Input/output stream
--     }


-- type ServerApp = PendingConnection -> IO ()

-- runWebSocketsSnapWith
--   :: Snap.MonadSnap m
--   => WS.ConnectionOptions
--   -> WS.ServerApp
--   -> m ()
-- runWebSocketsSnapWith options app = do
--   rq <- Snap.getRequest
--   Snap.escapeHttp $ \tickle readEnd writeEnd -> do

--     thisThread <- myThreadId
--     stream <- WS.makeStream (Streams.read readEnd)
--               (\v -> do
--                   Streams.write (fmap BSBuilder.lazyByteString v) writeEnd
--                   Streams.write (Just BSBuilder.flush) writeEnd
--               )

--     done <- newIORef False

--     let options' = options
--                    { WS.connectionOnPong = do
--                         tickle (max 45)
--                         WS.connectionOnPong options
--                    }

--         pc = WS.PendingConnection
--                { WS.pendingOptions  = options'
--                , WS.pendingRequest  = fromSnapRequest rq
--                , WS.pendingOnAccept = forkPingThread tickle done
--                , WS.pendingStream   = stream
--                }
--     (app pc >> throwTo thisThread ServerAppDone) `finally` writeIORef done True
