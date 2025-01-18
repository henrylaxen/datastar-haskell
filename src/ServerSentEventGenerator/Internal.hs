module ServerSentEventGenerator.Internal where

-- import           Constants
import Data.ByteString.Builder
    ( toLazyByteString, byteString, intDec, lazyByteString, Builder )
import Data.ByteString.Lazy.UTF8
    ( ByteString, fromString, toString )
import Data.Default ( Default(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Maybe ( catMaybes )
-- import Data.String
import Data.Text ( Text )
import qualified Data.Text.Encoding as T ( encodeUtf8 )
import Control.Exception
import Debug.Trace

class Monad m => HttpVersion m where
  isHttpVersion1_1 :: m Bool

instance HttpVersion Identity  where
  isHttpVersion1_1 = return True

class Monad m => Sender m where
  send :: Builder -> m ()

instance Sender IO where
  send = putStrLn . builderToString

instance Sender Identity where
  send x = Identity (trace (builderToString x) ())

watch :: Builder -> ()
watch x = runIdentity (send x)  

class ToBuilder a where
  toBuilder :: a -> Builder

instance ToBuilder ByteString where
  toBuilder = lazyByteString

instance ToBuilder Text where
  toBuilder = byteString . T.encodeUtf8

instance ToBuilder String where
  toBuilder = lazyByteString . fromString

instance ToBuilder Bool where
  toBuilder False = "false"
  toBuilder True  = "true"

instance ToBuilder Int where
  toBuilder = intDec

-- | convert a Builder to a String, mainly for debugging
builderToString :: Builder -> String
builderToString = toString . toLazyByteString

-- I created this silly function to prevent the Orphan instance
-- warning for defining an Eq instance for Builders using show
buildersMatch :: [Builder] -> [Builder] -> Bool
buildersMatch x y = show x == show y

-- | append linefeeds to list of Builders and add another one to the end

withLineFeeds :: [Builder] -> Builder
withLineFeeds = mconcat . map (<> "\n" )

-- | A convenience function which turns default values into Nothings and values not matching
--   the default into Just Builders, adding a space to the end of a Just case.

adjustSpaces :: Builder -> Builder
adjustSpaces x = if buildersMatch [mempty] [x] then mempty  else x <> " " 

maybeDefault :: (Eq a, Default a, ToBuilder a) => Builder -> Maybe a -> Maybe Builder
maybeDefault _ Nothing =  Nothing
maybeDefault prefix (Just x) = if buildersMatch [(toBuilder x)] [(prefix)]
  then Nothing
  else Just ("data: " <> adjustSpaces prefix <> toBuilder x)

-- | Takes a list of Maybe Builder, throws away the Nothings, and appends line feeds
--   to the rest, removing the Justs

format :: [Maybe Builder] -> Builder
format x = (withLineFeeds . catMaybes) x <> "\n"

mapWithData :: Builder -> [Builder] -> [Maybe Builder]
mapWithData prefix = map (Just . (("data: " <> adjustSpaces prefix) <>)) 

withEvent :: ToBuilder a => a -> Builder
withEvent = (<>) "event: " . toBuilder

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
