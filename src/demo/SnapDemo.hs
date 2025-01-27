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
import           ServerSentEventGenerator
-- import           ServerSentEventGenerator.Types


type Tickle = (Int -> Int) -> IO ()

sseRun :: SSEapp -> Snap ()
sseRun (SSEapp app) = do
  request <- Snap.getRequest
  let lastId = getHeader "Last-Event-ID" request
  headers <- sseHeaders

  Snap.escapeHttp $ \tickle _ writeEnd -> do
      putStrLn ("Last-Event-ID: " <> show lastId)
      pingThreadId <-forkIO (ping tickle writeEnd)
      handle (handleException pingThreadId "sseRun") $ do
        Streams.write (Just headers) writeEnd
        Streams.write (Just flush) writeEnd
        putStrLn "enter app"
        app writeEnd
        putStrLn "exit app"
        killThread pingThreadId
        putStrLn "killing ping thread"
        Streams.write Nothing writeEnd
        putStrLn "sseRun done"

sseWrite :: Builder-> SSEstream -> IO ()
sseWrite x writeEnd = do
  let s = decode . unpack . toLazyByteString $ x
  putStrLn ("sseWrite: " <> s)
  Streams.write (Just x) writeEnd
  Streams.write (Just flush) writeEnd

ping :: Tickle -> SSEstream -> IO ()  
ping tickle writeEnd = forever $ do
  pingThreadId <- myThreadId
  putStrLn ("PING: " <> show pingThreadId)
  handle (handleException pingThreadId "ping") $ do
    Streams.write  (Just ":\n\n") writeEnd
    Streams.write  (Just flush) writeEnd
  tickle (max 60)
  threadDelay (11 * 1000 * 1000)

--   print ("ping" :: Builder)
--   (Streams.write  (Just ":\n\n")) writeEnd
--     `catch` (\(e :: SomeException) -> print e >> return ())
--     -- as near as I can tell, this never triggers
--   tickle (max 60)
--   threadDelay (11 * 1000 * 1000)

instance HttpVersion Snap  where
  isHttpVersion1_1 = do
    version <- rqVersion <$> getRequest
    return (version == (1,1))

handleException :: ThreadId -> String -> SomeException -> IO ()
handleException t s e = do
  killThread t
  putStrLn (s <> ": " <> displayException e)
  throwIO e

