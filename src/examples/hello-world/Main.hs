module Main where

import Control.Applicative ( Alternative((<|>)) )
import Control.Concurrent ( threadDelay )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import ServerSentEventGenerator
import ServerSentEventGenerator.Server.Snap
import Snap
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.IO
import Control.Exception
import Data.Aeson
import Data.Default
import Control.Monad ( foldM_ )
import Snap.Util.FileServe ( serveDirectory )


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  indexText <- T.readFile "src/examples/hello-world/hello-world.html"
  simpleHttpServe (defaultConfig :: Config Snap a) (site indexText)
    `catch` (\(e :: SomeException) -> print e)
site :: Text -> Snap ()
site indexText =
  ifTop ((liftIO $ T.readFile "src/examples/hello-world/hello-world.html") >>= writeText)
  <|>
    route [
        ("favicon.ico" , return ())
      , ("hello-world" , handlerHelloWord)
      ]

newtype Delay = Delay Int
  deriving Show

instance FromJSON Delay where
    parseJSON = withObject "Delay" $ \v -> Delay
        <$> v .: "delay"

handlerHelloWord :: Snap ()
handlerHelloWord = do
--   req <- getRequest
--   (request,value) <- readSignals
  Just ds  <- getParam "datastar"
  let Just delay = decodeStrict ds :: Maybe Delay
  runSSE (SSEapp (f delay))
  where
    hello = "Hello, world!"
    merge x = mergeFragments ("<div id=\"message\">" <> x <> "</div>") def def def def
    f :: Delay -> SSEstream -> IO ()
    f (Delay n) w = foldM_ (\x -> foldSlowly x) mempty (T.unpack hello)
      where
        pause = threadDelay (n * 1000)
        foldSlowly b c = do
          pause
          let s = b <> (T.singleton c)
          send (merge s) w
          return s
