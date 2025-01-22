module ServerSentEventGenerator.Internal where

import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder
import System.IO
import Control.Concurrent.MVar

sp :: [Builder] -> IO ()
sp bs = do
  m <- newMVar ()
  mapM_ (print1 m) bs
  where
    print1 m b = do
      takeMVar m
      hPutBuilder stdout "\n----\n"
      hPutBuilder stdout b
      putMVar m ()

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

-- withNullDefault :: (ToBuilder a, Monoid b, ToBuilder b, Eq b, ToBuilder c, Eq c, Monoid c) =>
--                    a -> b -> c -> Builder
-- withNullDefault prefix defaultValueIfNull value =
--   front <> (if value == mempty then toBuilder defaultValueIfNull else toBuilder value)
--   where front = (cData <> ": " <> toBuilder prefix <> " ")


