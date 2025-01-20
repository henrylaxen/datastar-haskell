module ServerSentEventGenerator.Internal where

import ServerSentEventGenerator.Class
-- import ServerSentEventGenerator.Newtypes
import Data.ByteString.Builder
import Data.Default ( Default(..) )
import Data.Maybe ( catMaybes )
import Control.Exception

-- I created this silly function to prevent the Orphan instance
-- warning for defining an Eq instance for Builders using show
buildersMatch :: [Builder] -> [Builder] -> Bool
buildersMatch x y = show x == show y

-- | append linefeeds to list of Builders and add another one to the end

withLineFeeds :: [Builder] -> Builder
withLineFeeds = mconcat . map (<> "\n" )

-- | A convenience function which turns default values into Nothings and values not matching
--   the default into Just Builders, adding a space to the end of a Just case.

-- adjustSpaces :: Builder -> Builder
-- adjustSpaces x = if buildersMatch [mempty] [x] then mempty  else x <> " " 

-- | Takes a list of Maybe Builder, throws away the Nothings, and appends line feeds
--   to the rest, removing the Justs

format :: [Maybe Builder] -> Builder
format x = (withLineFeeds . catMaybes) x <> "\n"

-- mapWithData :: Builder -> [Builder] -> [Maybe Builder]
-- mapWithData prefix bs = map (Just . (("data: " <> adjustSpaces prefix) <>)) bs

-- check to see if empty data lines are allowed?
-- mapWithData :: Builder -> [Builder] -> [Maybe Builder]
-- mapWithData prefix bs =
--   if null bs
--   then (error "mapWithData has no date")
--   else map (Just . (("data: " <> adjustSpaces prefix) <>)) bs

withEvent :: ToBuilder a => a -> Builder
withEvent = (<>) "event: " . toBuilder

withRequired :: (DsCommand a, Default a, Eq a, ToBuilder a) => ServerSentEventGeneratorExceptions -> a -> Maybe Builder
withRequired exception value = if toBuilder value == mempty
  then bug exception
  else withDefault value

-- | if the value is equal to the default value, replace it with Nothing,
--   which will later be eliminated from the list of Builders.
--   if they aren't equal, wrap the result in a Just and add "data:
--   the message 
-- withDefault ::(Default a, Eq a, ToBuilder a) => Builder -> a -> Maybe Builder
-- withDefault prefix value = if value == def
--   then Nothing
--   else Just ("data: " <> prefix <> " " <> toBuilder value)

withDefault ::(DsCommand a, Default a, Eq a, ToBuilder a) => a -> Maybe Builder
withDefault value = if value == def
  then Nothing
  else Just (dsCommand value <> toBuilder value)

withBuilderList :: (DsCommand a, ToBuilderList a) =>  a -> [Maybe Builder]
withBuilderList s = map (Just . ((dsCommand s) <>)) (toBuilderList s)

data ServerSentEventGeneratorExceptions =
   RemoveFragmentSelectorIsMissing 
 | SignalsSelectorIsMissing        
 | RemoveSignalsPathIsMissing      
 | RemoveSignalsPathIsEmpty        
 | ExecuteScriptIsMissing          
instance Exception ServerSentEventGeneratorExceptions

instance Show ServerSentEventGeneratorExceptions where
 show RemoveFragmentSelectorIsMissing = "The selector field is required in RemoveFragment"
 show SignalsSelectorIsMissing        = "The selector field is required in MergeSignals"
 show RemoveSignalsPathIsMissing      = "The path field is required in RemoveSignals"
 show RemoveSignalsPathIsEmpty        = "The path field cannot be an empty list"
 show ExecuteScriptIsMissing          = "The script field is required in ExecuteScript"

bug :: Exception e => e -> a
bug = throw
