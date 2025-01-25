module Main where


import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString
import           Data.Maybe
import           Data.Text
import           Data.Text.Encoding
import           Data.Time           ( getCurrentTime )
import qualified HTMLEntities.Text   ( text )
import           NeatInterpolation   hiding (text)
import           S
import           Snap
import           Snap.Util.FileServe ( serveDirectory )



-- handlerFeed :: Snap ()
-- handlerFeed = do
--   sseOpen loop
--   where
--     timeNow :: DsString -> DsString
--     timeNow x = [i|<div id="feed">The time is: ${x}</div>|]
--     loop :: SSEstream -> IO ()Luc
--     loop sseStream = do
--       now <- (encodeUtf8 :: Text -> DsString)  . show <$> liftIO getCurrentTime
--       let dsStr = makeDatastar MergeFragments [timeNow now] Nothing
--       sseWrite sseStream dsStr
--       threadDelay 2000000
--       loop sseStream

main :: IO ()
main = do
  indexFile <- decodeUtf8 <$> Data.ByteString.readFile "www/index.html"
  let
    index     = replace "<replacedByThisPageHere>" (replacement indexFile) indexFile
    mbPort    = getPort (defaultConfig :: Config Snap a)
    newConfig = setPort (fromMaybe 8000 mbPort) (defaultConfig :: Config Snap a)
  conf <- commandLineConfig newConfig
  print conf
  simpleHttpServe conf (site index)
  where
    replacement :: Text -> Text
    replacement x = "<pre>" <> HTMLEntities.Text.text x <> "</pre>"

site :: Text -> Snap ()
site index =
    ifTop (writeText index) <|>
    route [
        ("favicon.ico" , return ())
      , ("feed"        , handlerFeed)
      ] <|> serveDirectory "www"

handlerFeed :: Snap ()
handlerFeed = do
  liftIO $ putStrLn "feeding"
  sseRun f

--   <div data-text="input.value">
--   <input data-bind-input type="text" />
--   <button data-class="{hidden: input.value == '', bold: input.value == 1}">Hello</button>
-- event: datastar-merge-fragments
-- data: <div id="feed">XXXXXXXXXXXXXXXXXXXXXXXXX</div>


d :: Text
d = [trimming|
Cache-control: no-cache
Content-type: text/event-stream
Connection: keep-alive
event: datastar-merge-fragments
data: fragments <div id="feed">XXXXXXXXXXXXXXXXXXXXXXXXX</div>
|] <> "\n\n"


f :: SSEapp
f = SSEapp (\w -> do
  print ("In SSEapp" :: Text)
  sseWrite (encodeUtf8Builder d) w )

  
