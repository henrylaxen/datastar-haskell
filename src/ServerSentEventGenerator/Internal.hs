module ServerSentEventGenerator.Internal where

import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Newtypes
import Data.ByteString.Builder
import Data.Default ( Default(..) )
import Data.Maybe ( catMaybes )
import Control.Exception

-- I created this silly function to prevent the Orphan instance
-- warning for defining an Eq instance for Builders using show
buildersMatch :: [Builder] -> [Builder] -> Bool
buildersMatch x y = show x == show y

-- | append linefeeds to list of Builders and add another one to the end

withLineFeeds :: [SB] -> SB
withLineFeeds = mconcat . map (<> "\n" )

-- | A convenience function which turns default values into Nothings and values not matching
--   the default into Just SBs, adding a space to the end of a Just case.

adjustSpaces :: SB -> SB
adjustSpaces x = if buildersMatch [mempty] [x] then mempty  else x <> " " 

maybeDefault :: (Eq a, Default a, ToBuilder a) => SB -> Maybe a -> Maybe Builder
maybeDefault _ Nothing =  Nothing
maybeDefault prefix (Just x) = if buildersMatch [(toBuilder x)] [(prefix)]
  then Nothing
  else Just ("data: " <> adjustSpaces prefix <> toBuilder x)

-- | Takes a list of Maybe Builder, throws away the Nothings, and appends line feeds
--   to the rest, removing the Justs

format :: [Maybe SB] -> SB
format x = (withLineFeeds . catMaybes) x <> "\n"

mapWithData :: SB -> [SB] -> [Maybe SB]
mapWithData prefix bs =
  if null bs
  then (error "mapWithData has no date")
  else map (Just . (("data: " <> adjustSpaces prefix) <>)) bs

withEvent :: ToBuilder a => a -> SB
withEvent = (<>) "event: " . toBuilder

withDefault ::(Default a, Eq a, ToBuilder a) => SB -> a -> Maybe SB
withDefault prefix x = if x == def
  then Nothing
  else Just ("data: " <> adjustSpaces prefix <> toBuilder x)

data ServerSentEventGeneratorExceptions =
   RemoveFragmentSelectorIsMissing String
 | SignalsSelectorIsMissing        String
 | RemoveSignalsPathIsMissing      String
 | RemoveSignalsPathIsEmpty        String
 | ExecuteScriptIsMissing          String
  deriving Show
instance Exception ServerSentEventGeneratorExceptions

bug :: ServerSentEventGeneratorExceptions -> a
bug (RemoveFragmentSelectorIsMissing _) =
  throw (RemoveFragmentSelectorIsMissing "the selector is required in RemoveFragment")
bug (SignalsSelectorIsMissing _) =
  throw (SignalsSelectorIsMissing "the selector is required in MergeSignals")
bug (RemoveSignalsPathIsMissing _) = 
  throw (RemoveSignalsPathIsMissing "the path is required in RemoveSignals")
bug (RemoveSignalsPathIsEmpty _) =
  throw (RemoveSignalsPathIsEmpty "the path cannot be an empty list")
bug (ExecuteScriptIsMissing _) =
  throw (ExecuteScriptIsMissing "the script is required in ExecuteScript")
