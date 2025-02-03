{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

-- import Control.Applicative ( Alternative((<|>)) )
-- import Control.Concurrent ( threadDelay )
-- import Control.Monad ( foldM_ )
-- import Control.Monad.IO.Class ( MonadIO(liftIO) )
-- import Data.ByteString.Lazy
-- import Data.Maybe ( fromMaybe )
-- import Data.Time ( getCurrentTime )
import ServerSentEventGenerator.Types
-- import ServerSentEventGenerator.Constants
-- import Snap
-- import System.IO
--     ( stdout, hSetBuffering, stderr, BufferMode(NoBuffering) )
-- import qualified Data.Text as T
-- import Data.Aeson.Types hiding ( Options )
-- import Data.Aeson.KeyMap
-- import qualified Data.Aeson.Key as Key
-- import qualified Data.Vector as V
import NeatInterpolation
-- import Data.Aeson hiding ( Options )
import Data.Default ( Default(def) )
-- import Data.Text
import ServerSentEventGenerator
-- import Data.String

-- import Text.Show.Pretty (pPrint)

xmain :: IO ()
xmain = return ()
  
main :: IO ()
main = do
  let
    yourOptions = O "event1" 2000
    oneTwo      = [trimming|{"one":1,"two":2}|]
    firstSecond = [trimming|{"one":"first\\n signal","two":"second signal"}|]
    paths1      = ["one"]
    paths2      = ["one", "two.alpha"]
    script1     = "console.log('hello')"
    script2     = "if (true) {\n  console.log('hello');\n}"
    attributes1 = "type: text/javascript\nblocking: false"
    attributes2 = "type: module"
  test [
      mergeFragments "<div>Merge</div>" (SEL "div") Append (FO 1000 True) yourOptions
    , mergeFragments "<div>Merge</div>" def Morph (FO 1000 True) def
    , mergeFragments "<div>\n  <span>Merge</span>\n</div>" def def def def
    , mergeFragments "<div>Merge</div>" def def def def
    , removeFragments (SEL  "#target") (FO 2000 True) yourOptions
    , removeFragments (SEL  "#target") (FO 300  False) def
    , removeFragments (SEL  "#target") def def
    , mergeSignals oneTwo True yourOptions
    , mergeSignals firstSecond True def
    , mergeSignals oneTwo def def
    , removeSignals paths2 yourOptions
    , removeSignals paths1 def
    , executeScript script1 attributes1 (Auto False) yourOptions
    , executeScript script1 attributes2 (Auto True)  def
    , executeScript script2 def def def
    ]

