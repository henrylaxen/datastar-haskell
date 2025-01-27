module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text ( Text )
import           Data.Time           ( getCurrentTime )
import qualified HTMLEntities.Text   ( text )
import           Snap
import           Snap.Util.FileServe ( serveDirectory )
import           System.IO hiding (putStr)
import ServerSentEventGenerator
import SnapDemo
import Data.Default
-- import qualified Data.Map as M
-- import Data.ByteString.Lazy (ByteString)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  indexFile <- T.readFile "src/demo/www/index.html"
  let
    indexText = T.replace "<replacedByThisPageHere>" (replacement indexFile) indexFile
    mbPort    = getPort (defaultConfig :: Config Snap a)
    newConfig = setPort (fromMaybe 8000 mbPort) (defaultConfig :: Config Snap a)
  conf <- commandLineConfig newConfig
  print conf
  simpleHttpServe conf (site indexText)
  where
    replacement :: Text -> Text
    replacement x = "<pre>" <> HTMLEntities.Text.text x <> "</pre>"

site :: Text -> Snap ()
site indexText =
    ifTop (writeText indexText) <|>
    route [
        ("favicon.ico" , return ())
      , ("feed"        , handlerFeed)
      , ("keats"       , handlerKeats)
      , ("signals"     , handlerSignals)
      ] <|> serveDirectory "src/demo/www"

handlerSignals  :: Snap ()
handlerSignals = do
  req    <- T.pack . show <$> getRequest
  body   <- T.pack . show <$> readRequestBody 1024
  params <- T.pack . show <$> getParams
  let
--     jsMap = paramsAsText req
    output = mconcat [
        "\nRequest\n"
      , req
      , "\nParams\n"
      , params
      , "\nBody\n"
      , body
      , "\nEnd\n" ]
    ds = mergeFragments (toPre output) (SEL "#signals") Inner def def
  liftIO $ putStrLn (T.unpack output)
  sseRun (SSEapp (sseWrite ds))

handlerFeed :: Snap ()
handlerFeed = do
  liftIO $ putStrLn "feeding"
  sseRun (SSEapp f)
  where
    f :: SSEstream -> IO ()
    f w = do
      putStrLn "Enter SSEapp"
      let x10times = [1..10] :: [Int]
      putStrLn "Write 10 times"
      mapM_ (writeNow w) x10times

      writeBoth sleeping w
      sleep 70
      putStrLn "Wake up"
      putStrLn "Write 10 times"
      mapM_ (writeNow w) x10times
      
      writeBoth allDone w
      sleep 2
      sseWrite removeDstar w
    writeNow :: SSEstream -> Int -> IO ()
    writeNow w n = do
      now <- getCurrentTime >>=
        return . T.pack . ((Prelude.replicate n '.') <> ) . show
      sseWrite (feedDstar now) w
      threadDelay (1 * 1000 * 1000)
    writeBoth x w = putStrLn (T.unpack x) >> sseWrite (feedDstar x) w
    sleeping = "Sleeping for 70 seconds, but continuing to ping"
    allDone  = "All Done"
    feedDstar :: Text -> Text
    feedDstar x = mergeFragments ["<div id=\"feed\"><b>" <> x <> "</b></div>"] def def def def
    removeDstar :: Text
    removeDstar = removeFragments (SEL "#explain") (FO 5000 def) def

handlerKeats :: Snap ()
handlerKeats = do
  liftIO $ putStrLn "Keats"
  ode <- liftIO  $ T.readFile "src/demo/www/keats.txt"
  sseRun (SSEapp (f ode))
  where
    f ::  Text -> SSEstream -> IO ()
    f ode w =  singleThreaded $ foldM_ (\x -> foldSlowly w x) mempty (T.unpack ode)
    keatsDstar :: Text -> Text
    keatsDstar x = mergeFragments (toPre x) (SEL "#keats") Inner def def
    foldSlowly :: SSEstream -> Text ->  Char -> IO Text
    foldSlowly w b c = do
      pause
      let s = b <> (T.singleton c)
      sseWrite (keatsDstar s) w
      return s

pause :: IO ()
pause = threadDelay (10 * 100 * 100 `div` 2)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000 * 1000)

