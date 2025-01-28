module ServerSentEventGenerator.Internal where

import Control.Concurrent
import Control.Exception
import Data.Text hiding ( map )
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import           Data.Functor.Identity     ( Identity(..) )


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

-- | Insert "data: " and the given text in front of each element of the list
-- | >>> withList "fragments" ["l1","l2"]
--   ["data: fragments l1","data: fragments l2"]

withList :: Text -> [Text] -> [Text]
withList name =  Prelude.map (\x -> cData <> ": " <> name <> " " <> x)

-- | Send a list of Texts, as a unit (single treaded) to the server dependent
--   sse function, which is the sole method of the SSE class

sendM :: Text -> IO ()
sendM b = singleThreaded (sse b)

singleThreaded :: IO () -> IO ()
singleThreaded action = bracket 
    (newMVar ()) 
    (\mvar -> putMVar mvar ())
    (\mvar -> takeMVar mvar >> action)

test :: [Text] -> IO ()
test = mapM_ sendM

-- | A handy little helper to watch the result of sending stuff through sse
watch ::  Text -> ()
watch x = runIdentity (sse x)

