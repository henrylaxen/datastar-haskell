module Main where


import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
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
  liftIO $ putStrLn "feeding"
  sseRun (SSEapp f)
  where
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
      sseWrite (feedDstar now) w
      threadDelay (1 * 1000 * 1000)
    feedDstar :: Builder -> Builder
    feedDstar x = mconcat [
        "event: datastar-merge-fragments\n"
      , "data: mergeMode inner\n"
      , "data: fragments <div id=\"feed\">"        
      , "data: fragments <div>"
      , x
      , "</div>"
      ] <> "\n\n"
--      , "data: fragments <div id=\"feed\">"

      

handlerKeats :: Snap ()
handlerKeats = do
  liftIO $ putStrLn "Keats"
  ode <-liftIO $  Prelude.readFile "www/keats.txt"
--  let ode = "abc" :: String
  sseRun (SSEapp (f ode))
  where
    f :: String -> SSEstream -> IO ()
    f ode w = do
--      return ()
--      mapM_ (\x -> sseWrite (keatsDstar (charUtf8 x)) w >> pause) ode
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
--      pause
      let s = b <> [c]
      sseWrite (stringUtf8 . keatsDstar $ s) w
      return s

pause :: IO ()
pause = threadDelay (10 * 100 * 100 `div` 2)
                    
-- foldM_  :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()



-- w ::  SSEstream
-- w = undefined
-- keatsDstar :: Builder -> Builder
-- keatsDstar = id

singleThreaded :: IO a -> IO a
singleThreaded action = bracket 
    (newMVar ()) 
    (\mvar -> takeMVar mvar) 
    (\_ -> action)

toPre :: String -> String
toPre = mconcat . Prelude.map oneLine . Prelude.lines
  where
   oneLine x = "data: fragments ." <> x <> "\n"
--    addDot (x:xs) = if x == ' '
--      then ('.' : x : xs)
--      else (x:xs)

    
