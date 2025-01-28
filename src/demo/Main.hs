module Main where

import Control.Applicative ( Alternative((<|>)) )
import Control.Concurrent ( threadDelay )
import Control.Monad ( foldM_ )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Default ( Default(def) )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( getCurrentTime )
import ServerSentEventGenerator
import Snap
import Snap.Util.FileServe ( serveDirectory )
import ServerSentEventGenerator.Server.Snap ( runSSE, send )
import System.IO
    ( stdout, hSetBuffering, stderr, BufferMode(NoBuffering) )
import qualified HTMLEntities.Text ( text )
import qualified Data.Text as T
    ( replace, pack, singleton, unpack )
import qualified Data.Text.IO as T ( readFile )

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
  runSSE (SSEapp (send ds))

handlerFeed :: Snap ()
handlerFeed = do
  liftIO $ putStrLn "feeding"
  runSSE (SSEapp f)
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
      send removeDstar w
    writeNow :: SSEstream -> Int -> IO ()
    writeNow w n = do
      now <- getCurrentTime >>=
        return . T.pack . ((Prelude.replicate n '.') <> ) . show
      send (feedDstar now) w
      threadDelay (1 * 1000 * 1000)
    writeBoth x w = putStrLn (T.unpack x) >> send (feedDstar x) w
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
  runSSE (SSEapp (f ode))
  where
    f ::  Text -> SSEstream -> IO ()
    f ode w =  singleThreaded $ foldM_ (\x -> foldSlowly w x) mempty (T.unpack ode)
    keatsDstar :: Text -> Text
    keatsDstar x = mergeFragments (toPre x) (SEL "#keats") Inner def def
    foldSlowly :: SSEstream -> Text ->  Char -> IO Text
    foldSlowly w b c = do
      pause
      let s = b <> (T.singleton c)
      send (keatsDstar s) w
      return s

pause :: IO ()
pause = threadDelay (10 * 100 * 100 `div` 2)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000 * 1000)

