module ServerSentEventGenerator.Internal where

-- import           Constants
import           Data.ByteString.Builder
import           Data.ByteString.Lazy.UTF8
import           Data.Default              ( Default(..) )
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Text                 ( Text )
import qualified Data.Text.Encoding        as T

-- import qualified Data.ByteString.Lazy      as B
-- import qualified Data.Text                 as T

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

-- | prints a Builder on STDOUT
putBuilder :: Builder -> IO ()
putBuilder = putStrLn . builderToString

-- | convert a Builder to a String, mainly for debugging

builderToString :: Builder -> String
builderToString = toString . toLazyByteString

-- | append linefeeds to list of Builders and add another one to the end

withLineFeeds :: [Builder] -> Builder
withLineFeeds = mconcat . map (<> "\n" )

-- | A convenience function which turns default values into Nothings

maybeDefault
  :: (Eq a, Default a, ToBuilder a) => Maybe a -> Maybe Builder
maybeDefault Nothing =  Nothing
maybeDefault (Just x) = if x == def then Nothing else (Just . toBuilder $ x)

-- | Takes a list of Maybe Builder, throws away the Nothings, and appends line feeds
--   to the rest, removing the Justs

format :: [Maybe Builder] -> Builder
format = withLineFeeds . catMaybes 
