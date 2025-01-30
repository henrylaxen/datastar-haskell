module ServerSentEventGenerator.Internal where

import Control.Concurrent ( newMVar, putMVar, takeMVar )
import Control.Exception ( bracket )
-- import Data.Functor.Identity ( Identity(..) )
import Data.Text ( Text, pack )
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.String
import qualified Data.Text.IO

-- | Combines a list of Texts into a single Text, using the same mechanism
--   as the more commonly known functions wunWrds or unLines.  A line feed is
--   inserted between each builder in the list.  Empty builders are removed, so
--   there are no blank lines.

buildLines :: (Eq a, Monoid a, IsString a) => [a] -> [a]
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

prefixed :: StringLike a => a -> a -> a
prefixed name =  ( (cData <> cSColon <> name <> cSpace) <> )

withDefault :: StringLike a => a -> a -> a -> a
withDefault dStarEvent defaultValue value =
  if value == defaultValue || value == mempty
  then mempty
  else prefixed dStarEvent value
--  else cData <> cSColon <> dStarEvent <> cSpace <>  value

-- | Insert "data: " and the given text in front of each element of the list
-- | >>> withList "fragments" ["l1","l2"]
--   ["data: fragments l1","data: fragments l2"]

withList :: StringLike a => a -> [a] -> [a]
withList name =  Prelude.map (prefixed name)

singleThreaded :: IO () -> IO ()
singleThreaded action = bracket
    (newMVar ())
    (\mvar -> putMVar mvar ())
    (\mvar -> takeMVar mvar >> action)

-- | Handy little helpers to watch the result of sending stuff through sse
-- watch ::  Text -> ()
-- watch ::  IsString a => a -> ()
-- watch x = runIdentity (sse x)

test :: [Text] -> IO ()
test = mapM_ ps

-- sendM :: Monad m => [Text] -> m ()
-- -- sendM :: IsString a => [a] -> IO ()
-- sendM ts =  singleThreaded (mapM_ sse ts)

ps :: Text ->  IO ()
ps =  Data.Text.IO.putStr . pack . show
