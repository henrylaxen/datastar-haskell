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
  else "data: " <> toBuilder dStarEvent <> " " <>  toBuilder value

withFragments :: (ToBuilder a) => [a] -> [Builder]
withFragments =  map (\x -> cData <> ": " <> cFragments <> " " <> toBuilder x)




-- xwithDefault ::(Eq a, ToBuilder a) => Builder -> a -> a -> Maybe Builder
-- xwithDefault prefix d value = if value == d
--   then Nothing
--   else withPrefix prefix value

-- withDefault :: (Eq a, ToBuilder a, ToBuilder b) => b -> a -> a -> Maybe Builder
-- withDefault eventType defaultValue value = if value == defaultValue
--   then Nothing
--   else Just $ toBuilder eventType <> " " <>  toBuilder value

-- -- withColon :: (Semigroup a, Data.String.IsString a) => a -> a
-- withColon :: (ToBuilder a) => a -> Builder
-- withColon x = toBuilder x <> ": "

-- withJust :: (ToBuilder a) => a -> Maybe Builder
-- withJust x = Just  (toBuilder x <> "\n")

-- withData :: ToBuilder a => a -> Maybe Builder
-- withData = withPrefix cData

-- withEvent :: ToBuilder a => EventType -> a -> Maybe Builder
-- withEvent e a = Just ((toBuilder e) <> toBuilder a <> "\n")

-- withOptionalEvent :: (ToBuilder a, ToBuilder b) -> 

-- withPrefix :: (ToBuilder a, ToBuilder b) => a -> b -> Maybe Builder
-- withPrefix prefix x = Just $ (toBuilder prefix) <> ": " <> toBuilder x <> "\n"


-- withBuilderList :: (ToBuilderList a) =>  a -> [Maybe Builder]
-- withBuilderList s = map (Just . ((dsCommand s) <>)) (toBuilderList s)

-- buildLine :: [Builder] -> Builder
-- buildLine builders = (go mempty builders) <> "\n"
--   where
--     go _ [] = bug BuildLineDataIMissing
--     go acc [x] = acc <> x
--     go acc (b:bs) = b <> " " <> go acc bs

 
-- t = buildLine ["a"]

-- eventWithDefault :: (Eq a, ToBuilder a, ToBuilder b, ToBuilder c) => c -> b -> a -> a -> Builder
-- eventWithDefault eventType dStarEvent defaultValue value = if value == defaultValue
--   then mempty
--   else toBuilder eventType <> ": " <> toBuilder dStarEvent <> " " <>  toBuilder value

-- dataWithDefault :: (Eq a, ToBuilder a, ToBuilder b) => b -> a -> a -> Builder
-- dataWithDefault = eventWithDefault cData

-- -- I created this silly function to prevent the Orphan instance
-- -- warning for defining an Eq instance for Builders using show
-- buildersMatch :: [Builder] -> [Builder] -> Bool
-- buildersMatch x y = show x == show y

-- -- | append linefeeds to list of Builders and add another one to the end

-- withLineFeeds :: [Builder] -> Builder
-- withLineFeeds = mconcat . map (<> "\n" )

-- -- | A convenience function which turns default values into Nothings and values not matching
-- --   the default into Just Builders, adding a space to the end of a Just case.

-- -- adjustSpaces :: Builder -> Builder
-- -- adjustSpaces x = if buildersMatch [mempty] [x] then mempty  else x <> " " 

-- -- | Takes a list of Maybe Builder, throws away the Nothings, and appends line feeds
-- --   to the rest, removing the Justs

-- format :: [Maybe Builder] -> Builder
-- format x = (withLineFeeds . catMaybes) x <> "\n"

-- -- mapWithData :: Builder -> [Builder] -> [Maybe Builder]
-- -- mapWithData prefix bs = map (Just . (("data: " <> adjustSpaces prefix) <>)) bs

-- -- check to see if empty data lines are allowed?
-- -- mapWithData :: Builder -> [Builder] -> [Maybe Builder]
-- -- mapWithData prefix bs =
-- --   if null bs
-- --   then (error "mapWithData has no date")
-- --   else map (Just . (("data: " <> adjustSpaces prefix) <>)) bs

-- withEvent :: ToBuilder a => a -> Builder
-- withEvent = (<>) "event: " . toBuilder

-- withRequired :: (DsCommand a, Default a, Eq a, ToBuilder a) => ServerSentEventGeneratorExceptions -> a -> Maybe Builder
-- withRequired exception value = if toBuilder value == mempty
--   then bug exception
--   else withDefault value

-- -- | if the value is equal to the default value, replace it with Nothing,
-- --   which will later be eliminated from the list of Builders.
-- --   if they aren't equal, wrap the result in a Just and add "data:
-- --   the message 
-- -- withDefault ::(Default a, Eq a, ToBuilder a) => Builder -> a -> Maybe Builder
-- -- withDefault prefix value = if value == def
-- --   then Nothing
-- --   else Just ("data: " <> prefix <> " " <> toBuilder value)

-- withDefault ::(DsCommand a, Default a, Eq a, ToBuilder a) => a -> Maybe Builder
-- withDefault value = if value == def
--   then Nothing
--   else Just (dsCommand value <> toBuilder value)

-- withBuilderList :: (DsCommand a, ToBuilderList a) =>  a -> [Maybe Builder]
-- withBuilderList s = map (Just . ((dsCommand s) <>)) (toBuilderList s)

-- data ServerSentEventGeneratorExceptions =
--    RemoveFragmentSelectorIsMissing 
--  | SignalsSelectorIsMissing        
--  | RemoveSignalsPathIsMissing      
--  | RemoveSignalsPathIsEmpty        
--  | ExecuteScriptIsMissing          
-- instance Exception ServerSentEventGeneratorExceptions

