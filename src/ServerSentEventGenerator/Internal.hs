module ServerSentEventGenerator.Internal where

import Control.Concurrent
import Control.Exception
import Data.Text hiding ( map )
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants

-- | Combines a list of Texts into a single Text, using the same mechanism
--   as the more commonly known functions wunWrds or unLines.  A line feed is
--   inserted between each builder in the list.  Empty builders are removed, so
--   there are no blank lines.

buildLines :: [Text] -> [Text]
buildLines texts = if (mconcat texts) == mempty then [] else [go mempty texts]
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
    wa        = "a"      :: Text
    wb        = "b"      :: Text
    prefix    = "prefix" :: Text
    enclose              :: Text -> Text
    enclose x = "[" <> x <> "]\n"
    them = map enclose [
         withDefault prefix wa wa
       , withDefault prefix wa wa
       , withDefault prefix wb wa
       , withDefault prefix wa wb
       , withDefault prefix "" ""
       , withDefault prefix wa ""
       , withDefault prefix "" wa ]
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

withDefault :: Text -> Text -> Text -> Text
withDefault dStarEvent defaultValue value =
  if value == defaultValue || value == mempty
  then mempty
  else cData <> ": " <> dStarEvent <> " " <>  value

withList :: Text -> [Text] -> [Text]
withList name =  Prelude.map (\x -> cData <> ": " <> name <> " " <> x)

-- | Send a list of Texts, as a unit (single treaded) to the server dependent
--   sse function, which is the sole method of the SSE class

sendM :: Text -> IO ()
sendM b = singleThreaded (sse b)

singleThreaded :: IO a -> IO a
singleThreaded action = bracket 
    (newMVar ()) 
    (\mvar -> takeMVar mvar) 
    (\_ -> action)



test :: [Text] -> IO ()
test = mapM_ sendM

rr :: String -> Int -> IO ()
rr x n = do
  let a = Prelude.concatMap (\y -> ("," <> x <> show y) ) [1 .. n]
  putStr . Prelude.drop 1 $ a
  putStrLn " :: Text"
  putStrLn (x <> " :: IO ()")
