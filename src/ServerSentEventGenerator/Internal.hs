module ServerSentEventGenerator.Internal where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.ByteString.Builder
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants

nil :: Builder
nil = mempty

nils :: [Builder]
nils = []

-- | Combines a list of Builders into a single Builder, using the same mechanism
--   as the more commonly known functions wunWrds or unLines.  A line feed is
--   inserted between each builder in the list.  Empty builders are removed, so
--   there are no blank lines.

buildLines :: [Builder] -> [Builder]
buildLines builders = if (mconcat builders) == mempty then [] else [go mempty builders]
  where
    go acc []     = acc
    go acc [x]    = x <> acc
    go acc [x,y]  = case [x,y] of
      ["",z] -> z <> acc
      [z,""] -> z <> acc
      [u,v] -> u <> "\n" <> v <> acc
    go acc (b:bs) = if b == mempty then go acc bs else b <> "\n" <> go acc bs

{- | >>> :{
do
  let
    wa        = "a"      :: Builder
    wb        = "b"      :: Builder
    prefix    = "prefix" :: Builder
    enclose              :: Builder -> Builder
    enclose x = "[" <> x <> "]\n"
    them = map enclose [
         withDefault prefix wa wa
       , withDefault prefix wa wa
       , withDefault prefix wb wa
       , withDefault prefix wa wb
       , withDefault prefix nil nil
       , withDefault prefix wa nil
       , withDefault prefix nil wa ]
  test them
 :}
[]
[]
[data: prefix a]
[data: prefix b]
[]
[]
[data: prefix a]
-}

withDefault :: (Eq a, ToBuilder a, ToBuilder b) => b -> a -> a -> Builder
withDefault dStarEvent defaultValue value =
  if value == defaultValue || toBuilder value == mempty
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

test :: [Builder] -> IO ()
test = mapM_ sendM

rr :: String -> Int -> IO ()
rr x n = do
  let a = concatMap (\y -> ("," <> x <> show y) ) [1 .. n]
  putStr . Prelude.drop 1 $ a
  putStrLn " :: Builder"
  putStrLn (x <> " :: IO ()")
