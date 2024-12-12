module WarpMain where

import Control.Concurrent ( threadDelay )
import Data.ByteString.Builder ( lazyByteString, Builder )
import Data.String.Here.Interpolated ( i )
import Data.Text ( replace )
import Data.Time ( getCurrentTime )
import Network.HTTP.Types ( status200 )
import Network.Wai
    ( responseLBS, responseStream, Request(rawPathInfo), Response, ResponseReceived )
import Network.Wai.Application.Static
    ( staticApp, defaultWebAppSettings )
import Network.Wai.Handler.Warp ( run )
import Relude
import SSE ( DsString, makeSSE, Command(MergeFragments) )
import qualified HTMLEntities.Text ( text )

app :: Text -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app index req respond = do
  case rawPathInfo req of
    "/"     -> handlerIndex index respond
    "/feed" -> handlerFeed respond 
    _       -> staticApp (defaultWebAppSettings "www") req respond

handlerIndex :: Text -> (Response -> b) -> b
handlerIndex index respond = do
  respond $ responseLBS
    status200
    [("Content-Type", "text/html")]
    (encodeUtf8 index)

feedLoop :: (Builder -> IO ()) -> IO () -> IO ()
feedLoop send flush = loop
  where
    loop = do
      now <- (encodeUtf8 :: Text -> DsString)  . show <$> liftIO getCurrentTime
      let
        timeNow :: DsString -> DsString
        timeNow x = [i|<div id="feed">The time is: ${x}</div>|]
        dsStr = makeSSE MergeFragments [timeNow now] Nothing
      send (lazyByteString dsStr) >> flush
      threadDelay 2000000  -- 2 seconds
      loop

handlerFeed :: (Response -> b) -> b
handlerFeed respond = do
  respond $ responseStream
    status200
    [("Content-Type", "text/event-stream")]
    feedLoop

warpMain :: IO ()
warpMain = do
  indexFile <- decodeUtf8 <$> readFileBS "www/index.html"
  let
    replacement x = "<pre>" <> HTMLEntities.Text.text x <> "</pre>"
    index = replace "<replacedByThisPageHere>" (replacement indexFile) indexFile
  putStrLn "Server starting on port 3000..."
  run 3000 (app index)

