module ServerSentEventGenerator.Internal where

-- import           Constants
import Data.ByteString.Builder
    ( toLazyByteString, byteString, intDec, lazyByteString, Builder )
import Data.ByteString.Lazy.UTF8
    ( ByteString, fromString, toString )
import Data.Default ( Default(..) )
import Data.Functor.Identity ( Identity )
import Data.Maybe ( catMaybes )
-- import Data.String
import Data.Text ( Text )
import qualified Data.Text.Encoding as T ( encodeUtf8 )

class Monad m => HttpVersion m where
  isHttpVersion1_1 :: m Bool

instance HttpVersion Identity  where
  isHttpVersion1_1 = return True

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

-- | append linefeeds to list of Builders and add another one to the end

withLineFeeds :: [Builder] -> Builder
withLineFeeds = mconcat . map (<> "\n" )

-- | A convenience function which turns default values into Nothings and values not matching
--   the default into Just Builders.  

maybeDefault
  :: (Eq a, Default a, ToBuilder a) => Builder -> Maybe a -> Maybe Builder
maybeDefault _ Nothing =  Nothing
maybeDefault prefix (Just x) = if x == def then Nothing else (Just $ prefix <> toBuilder x)

-- | Takes a list of Maybe Builder, throws away the Nothings, and appends line feeds
--   to the rest, removing the Justs

format :: [Maybe Builder] -> Builder
format x = (withLineFeeds . catMaybes) x <> "\n"

mapWithData :: [Builder] -> [Maybe Builder]
mapWithData = map (Just . ("data: " <>)) 

