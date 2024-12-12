module SnapMain where

import Constants ( DsString )
import Control.Concurrent ( threadDelay )
import Data.String.Here.Interpolated ( i )
import Data.Text ( replace )
import Data.Time ( getCurrentTime )
import Relude
import SSE ( makeSSE, Command(MergeFragments) )
import Sender.Snap ( sseOpen, sseWrite, SSEstream )
import Snap
import Snap.Util.FileServe ( serveDirectory )
import qualified HTMLEntities.Text ( text )

handlerFeed :: Snap ()
handlerFeed = do
  sseOpen loop
  where
    timeNow :: DsString -> DsString
    timeNow x = [i|<div id="feed">The time is: ${x}</div>|]
    loop :: SSEstream -> IO ()
    loop sseStream = do
      now <- (encodeUtf8 :: Text -> DsString)  . show <$> liftIO getCurrentTime
      let dsStr = makeSSE MergeFragments [timeNow now] Nothing
      sseWrite sseStream dsStr
      threadDelay 2000000
      loop sseStream

snapMain :: IO ()
snapMain = do
  indexFile <- decodeUtf8 <$> readFileBS "www/index.html"
  let
    index     = replace "<replacedByThisPageHere>" (replacement indexFile) indexFile
    mbPort    = getPort (defaultConfig :: Config Snap a)
    newConfig = setPort (fromMaybe 3000 mbPort) (defaultConfig :: Config Snap a)
  conf <- commandLineConfig newConfig
  print conf
  simpleHttpServe conf (site index)
  where
    replacement :: Text -> Text
    replacement x = "<pre>" <> HTMLEntities.Text.text x <> "</pre>"

site :: Text -> Snap ()
site index =
    ifTop (writeText $ index) <|>
    route [
        ("favicon.ico" , return ())
      , ("feed"        , handlerFeed)
      ] <|> (serveDirectory "www")

