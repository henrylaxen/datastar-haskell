module ServerSentEventGenerator.Internal where

import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder
import Control.Monad.IO.Class
import Control.Concurrent
import System.IO

-- | Combines a list of Builders into a single Builder, using the same mechanism
--   as the more commonly known functions wunWrds or unLines.  A line feed is
--   inserted between each builder in the list.  Empty builders are removed, so
--   there are no blank lines.

-- | >>> putStr "Hello\nWorld!"
-- Hello
-- World!


{- |  :{
sp = hPutBuilder stdout
wa = "a" :: Builder
wb = "b" :: Builder
wprefix = "prefix" :: Builder
nil = "" :: Builder
enclose :: Builder -> Builder
enclose x = "[" <> x <> "]\n"
w1 = withDefault wprefix wa wa
w2 = withDefault wprefix wa wa
w3 = withDefault wprefix wb wa
w4 = withDefault wprefix wa wb
w5 = withDefault wprefix nil nil
w6 = withDefault wprefix wa nil
w7 = withDefault wprefix nil wa
w8 = [enclose w1, enclose w2, enclose w3, enclose w4, enclose w5, enclose w6, enclose w7]
sp w8
:}
[]
[]
[data: wprefix wa]
[data: wprefix wb]
[]
[]
[data: wprefix wa]
-}

{- |  :{
:}
-}

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

test :: MonadIO m => [Builder] -> m ()
test = mapM_ sendM

rr :: String -> Int -> IO ()
rr x n = do
  let a = concatMap (\y -> ("," <> x <> show y) ) [1 .. n]
  putStr . Prelude.drop 1 $ a
  putStrLn " :: Builder"
  putStrLn (x <> " :: IO ()")



-- wa = "a" :: Builder
-- wb = "b" :: Builder
-- wprefix = "prefix" :: Builder
-- nil = "" :: Builder
-- enclose :: Builder -> Builder
-- enclose x = "[" <> x <> "]\n"
-- w1 = withDefault wprefix wa wa
-- w2 = withDefault wprefix wa wa
-- w3 = withDefault wprefix wb wa
-- w4 = withDefault wprefix wa wb
-- w5 = withDefault wprefix nil nil
-- w6 = withDefault wprefix wa nil
-- w7 = withDefault wprefix nil wa
-- w8 = [enclose w1, enclose w2, enclose w3, enclose w4, enclose w5, enclose w6, enclose w7]
-- w :: IO ()
-- w = mapM_ sp w8



-- []
-- []
-- [data: wprefix wa]
-- [data: wprefix wb]
-- []
-- []
-- [data: wprefix wa]

