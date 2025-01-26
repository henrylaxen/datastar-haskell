module S (
    SSEstream
  , SSEapp (..)
  , Tickle
  , sseRun
  , sseWrite
  ) where

import           Codec.Binary.UTF8.String
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Data.ByteString.Lazy
import           Snap                          hiding ( headers )
import qualified System.IO.Streams             as Streams
import           Threading

type SSEstream = Streams.OutputStream Builder

data SSEapp = SSEapp (MVar () -> SSEstream -> IO ())

-- putBoth :: Builder -> Streams.OutputStream Builder -> IO ()

type Tickle = (Int -> Int) -> IO ()

sseRun :: MonadSnap m => SSEapp -> m () 
sseRun (SSEapp app) = do -- app :: (MVar () -> SSEstream -> IO ())
  request <- Snap.getRequest
  let lastId = getHeader "Last-Event-ID" request
  Snap.escapeHttp $ \tickle _ writeEnd -> do
--   Snap.escapeHttp $ \tickle _ writeEnd -> 
--     bracket
--       (newMVar ())
--       (\mvar -> putMVar mvar ())
--       (runWith tickle lastId writeEnd)
--   where
--     runWith tickle lastId writeEnd mVar  = do
      mVar <- newMVar ()
      takeMVar mVar 
      putStrLn ("Last-Event-ID: " <> show lastId)
      pingThreadId <-forkIO (ping mVar tickle writeEnd)
      handle (handleException pingThreadId "sseRun") $ do
        putMVar mVar ()
        Streams.write (Just headers) writeEnd
        Streams.write (Just flush) writeEnd
        putStrLn "enter app"
        putMVar mVar ()
--         app mVar writeEnd
        void $ takeMVar mVar
        putStrLn "exit app"
        killThread pingThreadId
        putStrLn "killing ping thread"
        Streams.write Nothing writeEnd
        putStrLn "sseRun done"
  where
    headers :: Builder
    headers = "Cache-control: no-cache\nContent-type: text/event-stream\nConnection: keep-alive\n"
       
sseWrite :: MVar () -> Builder-> SSEstream -> IO ()
sseWrite mVar x writeEnd = do
--   bracket
--     (newMVar ())
--     (\mvar -> putMVar mvar ())
--     (runPing)
--   where
--     runPing mVar  = do
      let s = decode . unpack . toLazyByteString $ x
      void $ takeMVar mVar
      putStrLn ("sseWrite: " <> s)
      Streams.write (Just x) writeEnd
      Streams.write (Just flush) writeEnd

ping :: MVar () -> Tickle -> SSEstream -> IO ()
ping mVar tickle writeEnd = forever $ do
  pingThreadId <- myThreadId
  takeMVar mVar 
  putSingle mVar ("PING: " <> show pingThreadId)
  handle (handleException pingThreadId "ping") $ do
    Streams.write  (Just ":\n\n") writeEnd
    Streams.write  (Just flush) writeEnd
  tickle (max 60)
  putMVar mVar ()
  threadDelay (11 * 1000 * 1000)


handleException :: ThreadId -> String -> SomeException -> IO ()
handleException t s e = do
  killThread t
  putStrLn (s <> ": " <> displayException e)
  throwIO e


-- bracket
-- :: IO a         computation to run first ("acquire resource")
-- -> (a -> IO b)  computation to run last  ("release resource")
-- -> (a -> IO c)  computation to run in-between
-- -> IO c	 

