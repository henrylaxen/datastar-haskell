module Threading where

import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Control.Concurrent
import           Control.Concurrent.Async
import qualified System.IO.Streams             as Streams
import           System.IO

putSingle :: MVar () -> String -> IO ()
putSingle mVar x = do
  _ <- takeMVar mVar
  Prelude.putStr $ x <> "\n"
  putMVar mVar ()




putBoth :: Builder -> Streams.OutputStream Builder -> IO ()
putBoth b w = concurrently_
 (hPutBuilder stdout b)
 (Streams.write (Just b) w) >> Streams.write (Just flush) w
  

--- - concurrently_ :: IO a -> IO b -> IO ()

-- TextShow toString :: Builder -> String
-- Data.String.Builder build :: Builder -> String
-- Text.XML.Generator fromBuilder :: XmlOutput t => Builder -> t
-- Data.Serializer builder :: Serializer s => Builder -> s
-- Control.Monad.Catch displayException :: e -> String
-- CorePrelude displayException :: e -> String
-- Control.Exception.Safe displayException :: e -> String
-- Foundation displayException :: e -> String
-- Text.Pandoc.Shared stringify :: Walkable Inline a => a -> String
-- Data.GI.Base.ShortPrelude show :: a -> String
-- -- plus more results not shown, pass --count=20 to see more
-- hPutBuilder :: Handle -> Builder -> IO () 
