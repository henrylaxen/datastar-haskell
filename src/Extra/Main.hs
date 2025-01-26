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
import           S
import           Snap
import           Snap.Util.FileServe ( serveDirectory )
import           Data.ByteString.Builder
import           System.IO
import           Threading

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  indexFile <- decodeUtf8 <$> Data.ByteString.readFile "www/index.html"
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
      ] <|> serveDirectory "www"

handlerFeed :: Snap ()
handlerFeed = do
  liftIO $ putStrLn "handlerFeed"
  mVar <- liftIO (newMVar ())
  sseRun (SSEapp mVar feedApp)

feedApp :: MVar () -> SSEstream -> IO ()
feedApp mVar w = do
  putStrLn "feedApp "
  isEmptyMVar mVar >>= print
  putSingle mVar "Enter SSEapp"
  let x10times = [1..10] :: [Int]
  putSingle mVar "Write 10 times"
  mapM_ writeNow x10times
  writeBoth sleeping
  sleep 70
  putSingle mVar "Wake up"
  putSingle mVar "Write 10 times"
  mapM_ writeNow x10times
  writeBoth allDone
  sleep 2
  sseWrite mVar removeDstar w
  where
    writeNow :: Int -> IO ()
    writeNow n = do
      now <- getCurrentTime >>=
        return . encodeUtf8Builder . Data.Text.pack . ((Prelude.replicate n '.') <> ) . show
      sseWrite mVar (feedDstar now) w
      sleep 1
    feedDstar :: Builder -> Builder
    feedDstar x = mconcat [
        "event: datastar-merge-fragments\n"
      , "data: fragments <div id=\"feed\">"
      , "<b>" <> x <> "</b>"
      ] <> "\n\n"
    writeBoth x = putSingle mVar x >> sseWrite mVar (feedDstar (stringUtf8 x)) w
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
  liftIO $ putStrLn "handlerKeats"
  ode <-liftIO $  Prelude.readFile "www/keats.txt"
--  let ode = "abc" :: String
  sseRun (SSEapp (f ode))
  where
    f :: String -> MVar () -> SSEstream -> IO ()
    f ode mVar w = do
      foldM_ (\x -> foldSlowly mVar w x) mempty ode
    keatsDstar :: String -> String
    keatsDstar x = mconcat [
        "event: datastar-merge-fragments\n"
      , "data: selector #keats\n"
      , "data: mergeMode inner\n"
      , "data: fragments <pre>\n"
      , toPre x
      , "</pre>"
      ] <> "\n\n"
    foldSlowly :: MVar () -> SSEstream -> String ->  Char -> IO String
    foldSlowly mVar w b c = do
      pause
      let s = b <> [c]
      sseWrite mVar (stringUtf8 . keatsDstar $ s) w
      return s

pause :: IO ()
pause = threadDelay (10 * 100 * 100 `div` 2)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000 * 1000)

toPre :: String -> String
toPre = mconcat . Prelude.map oneLine . Prelude.lines
  where
   oneLine x = "data: fragments ." <> x <> "\n"

