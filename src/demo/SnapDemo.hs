{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SnapDemo
  (
    sseRun
  , sseWrite
  , readSignals
  ) where

import           Data.Aeson
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Snap hiding ( headers, HttpVersion )
import qualified System.IO.Streams       as Streams
import Data.ByteString.Builder.Extra
import           ServerSentEventGenerator
import           Data.Text.Encoding ( encodeUtf8Builder )
import           Data.Text ( Text, unpack )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type Tickle = (Int -> Int) -> IO ()

-- set to True to see the text sent to the client on stdout as well,
-- VERY handy for debugging your Datastar code

debug :: Bool
debug = False

ps :: String -> IO ()
ps x = if debug then putStrLn x else return ()

sseRun :: SSEapp -> Snap ()
sseRun (SSEapp app) = do
  request <- Snap.getRequest
  let lastId = getHeader "Last-Event-ID" request
  headers <- sseHeaders

  Snap.escapeHttp $ \tickle _ writeEnd -> do
      ps ("Last-Event-ID: " <> show lastId)
      pingThreadId <-forkIO (ping tickle writeEnd)
      handle (handleException pingThreadId "sseRun") $ do
        Streams.write (Just headers) writeEnd
        Streams.write (Just flush) writeEnd
        ps "enter app"
        app writeEnd
        ps "exit app"
        killThread pingThreadId
        ps "killing ping thread"
        Streams.write Nothing writeEnd
        ps "sseRun done"

sseWrite :: Text -> SSEstream -> IO ()
sseWrite x writeEnd = do
  let s =  Data.Text.unpack x
  ps ("sseWrite: " <> s)
  Streams.write (Just (encodeUtf8Builder x)) writeEnd
  Streams.write (Just flush) writeEnd

ping :: Tickle -> SSEstream -> IO ()
ping tickle writeEnd = forever $ do
  pingThreadId <- myThreadId
  ps ("PING: " <> show pingThreadId)
  handle (handleException pingThreadId "ping") $ do
    Streams.write  (Just ":\n\n") writeEnd
    Streams.write  (Just flush) writeEnd
  tickle (max 60)
  threadDelay (11 * 1000 * 1000)

instance HttpVersion Snap  where
  isHttpVersion1_1 = do
    version <- rqVersion <$> getRequest
    return (version == (1,1))

handleException :: ThreadId -> String -> SomeException -> IO ()
handleException t s e = do
  killThread t
  ps (s <> ": " <> displayException e)
  throwIO e

{- | >>> :{
do
  signalsAsJsonIO (Nothing, "{\"key1\":{\"one\":1,\"two\":2},\"key2\":\"string\"}")    >>= print
  signalsAsJsonIO (Just "[{\"key1\":{\"one\":1,\"two\":2},\"key2\":\"string\"}]", "")  >>= print
:}
Object (fromList [("key1",Object (fromList [("one",Number 1.0),("two",Number 2.0)])),("key2",String "string")])
Object (fromList [("key1",Object (fromList [("one",Number 1.0),("two",Number 2.0)])),("key2",String "string")])
-}

-- signalsAsJsonIO :: (Maybe (Map ByteString [ByteString]), ByteString) -> IO Value
signalsAsJsonIO (mbDS,body) = do
  -- if its url encoded, then we only care about one key, named "datastar", 
  -- and the value is a bytestring encoded list with one element
  case mbDS of
      Nothing -> do
        let result = decode body  :: Maybe Value
        maybe (throwIO (JsonBodyException (T.pack . show $ body) )) return result
      Just ds -> do -- we expect a bytestring of the form "[bytetring]"
        let         -- so throw away the first and last characters01234567891011
          txt :: Text
          txt = T.decodeUtf8 ds
        if T.length txt <= 2
          then throwIO (JsonTextTooShort txt)
          else do
            let
              shortened = T.tail . T.init $ txt
            case ((decodeStrictText shortened) :: Maybe Value) of
              Nothing -> throwIO $ JsonDecodingException txt
              Just v -> return v

signalsAsJson :: Snap Value
signalsAsJson = do
  mbDS   <- getParam "datastar"
  body   <- readRequestBody 1024
  liftIO $ signalsAsJsonIO (mbDS,body)

-- | Get the current request, and return the value (as per Aeson) of the datastar parameter
--   which may be in the query parameters for a GET, PuT, etc request, or in the request body
--   if we have a POST request

readSignals :: Snap (Request, Value)
readSignals = do
  req   <- getRequest
  value <- signalsAsJson
  return (req,value)

data SnapExceptions = 
    JsonBodyException       !Text
  | JsonTextTooShort        !Text
  | JsonDecodingException   !Text

  deriving Show
instance Exception SnapExceptions

