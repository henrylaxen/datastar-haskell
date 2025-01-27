module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.ByteString
import           Data.Maybe
import           Data.Text
import           Data.Text.Encoding
import           Data.Time           ( getCurrentTime )
import qualified HTMLEntities.Text   ( text )
import           Snap
import           Snap.Util.FileServe ( serveDirectory )
import           Data.ByteString.Builder
import           System.IO
import ServerSentEventGenerator.Internal
import ServerSentEventGenerator.Types
import SnapDemo
import System.Directory

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  getCurrentDirectory >>= print
  indexFile <- decodeUtf8 <$> Data.ByteString.readFile "src/demo/www/index.html"
  let
    indexText = replace "<replacedByThisPageHere>" (replacement indexFile) indexFile
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
      ] <|> serveDirectory "src/demo/www"

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
        return . encodeUtf8Builder . Data.Text.pack . ((Prelude.replicate n '.') <> ) . show
      sseWrite (feedDstar now) w
      threadDelay (1 * 1000 * 1000)
    feedDstar :: Builder -> Builder
    feedDstar x = mconcat [
        "event: datastar-merge-fragments\n"
      , "data: fragments <div id=\"feed\">"
      , "<b>" <> x <> "</b>"
      ] <> "\n\n"
    writeBoth x w = putStrLn x >> sseWrite (feedDstar (stringUtf8 x)) w
    removeDstar :: Builder
    removeDstar = mconcat [
        "event: datastar-remove-fragments\n"
      , "data: selector #explain\n"
      , "data: settleDuration 5000\n"
      , "data: useViewTransition true"
      ] <> "\n\n"            
    sleeping = "Sleeping for 70 seconds, but continuing to ping"
    allDone  = "All Done"

handlerKeats :: Snap ()
handlerKeats = do
  liftIO $ putStrLn "Keats"
  ode <-liftIO $  Prelude.readFile "src/demo/www/keats.txt"
--  let ode = "abc" :: String
  sseRun (SSEapp (f ode))
  where
    f :: String -> SSEstream -> IO ()
    f ode w = do
      singleThreaded $ foldM_ (\x -> foldSlowly w x) mempty ode
    keatsDstar :: String -> String
    keatsDstar x = mconcat [
        "event: datastar-merge-fragments\n"
      , "data: selector #keats\n"
      , "data: mergeMode inner\n"
      , "data: fragments <pre>\n"
      , toPre x
      , "</pre>"
      ] <> "\n\n"
    foldSlowly :: SSEstream -> String ->  Char -> IO String
    foldSlowly w b c = do
      pause
      let s = b <> [c]
      sseWrite (stringUtf8 . keatsDstar $ s) w
      return s

pause :: IO ()
pause = threadDelay (10 * 100 * 100 `div` 2)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000 * 1000)


toPre :: String -> String
toPre = mconcat . Prelude.map oneLine . Prelude.lines
  where
   oneLine x = "data: fragments ." <> x <> "\n"

