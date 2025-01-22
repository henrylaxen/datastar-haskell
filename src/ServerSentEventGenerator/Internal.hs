module ServerSentEventGenerator.Internal where

import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder
import Control.Monad.IO.Class
import Control.Concurrent

buildLines :: [Builder] -> Builder
buildLines builders = (go mempty builders)
  where
    go acc []     = acc
    go acc [x]    = x <> acc
    go acc [x,y]  = case [x,y] of
      ["",z] -> z <> acc
      [z,""] -> z <> acc
      [u,v] -> u <> "\n" <> v <> acc
    go acc (b:bs) = if b == mempty then go acc bs else b <> "\n" <> go acc bs

withDefault :: (Eq a, ToBuilder a, ToBuilder b) => b -> a -> a -> Builder
withDefault dStarEvent defaultValue value = if value == defaultValue
  then mempty
  else cData <> ": " <> toBuilder dStarEvent <> " " <>  toBuilder value

withList :: (ToBuilder a) => Builder -> [a] -> [Builder]
withList name =  map (\x -> cData <> ": " <> name <> " " <> toBuilder x)

  -- | Send a list of Builders, as a unit (single treaded) to the server dependent
--   sse function, which is the sole method of the SSE class

sendM :: MonadIO m => Builder -> m ()
sendM b = liftIO $ do
  m <- newMVar ()
  forkIO (send1 m) >> return ()
  where
    send1 m = do
      takeMVar m
      sse b
      putMVar m ()

test :: MonadIO m => [Builder] -> m ()
test = mapM_ sendM
