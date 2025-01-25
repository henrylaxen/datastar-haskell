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
import           S
import           Snap
import           Snap.Util.FileServe ( serveDirectory )
import           Data.ByteString.Builder

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
  sseRun (SSEapp f)

--   <div data-text="input.value">
--   <input data-bind-input type="text" />
--   <button data-class="{hidden: input.value == '', bold: input.value == 1}">Hello</button>
-- event: datastar-merge-fragments
-- data: <div id="feed">XXXXXXXXXXXXXXXXXXXXXXXXX</div>


d :: Text -> Text
d x = "event: datastar-merge-fragments\ndata: fragments " <> x <>   "\n\n"

f :: SSEstream -> IO ()
f w = do
  putStrLn "Enter SSEapp"
  let x10times = [1..10] :: [Int]
  putStrLn "Write 10 times"
  mapM_ (const $ writeNow w) x10times
  putStrLn "Sleep for 70 seconds"
  threadDelay (70 * 1000 * 1000)
  putStrLn "Wake up"
  putStrLn "Write 10 times"
  mapM_ (const $ writeNow w) x10times
  putStrLn "All done"
  
writeNow :: SSEstream -> IO ()
writeNow w = do
  now <- getCurrentTime >>= return . encodeUtf8Builder . Data.Text.pack . show
  sseWrite (ds now) w
  threadDelay (1 * 1000 * 1000)

ds :: Builder -> Builder
ds x = mconcat [
    "event: datastar-merge-fragments\n"
  , "data: fragments <div id=\"feed\">"
  , x
  , "</div>"
  ] <> "\n\n"
  
