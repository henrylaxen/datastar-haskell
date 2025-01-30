{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Class where

import Data.Default ( Default(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.ByteString.Builder
-- import qualified Data.Text as T
import Data.Text ( Text )
-- import qualified System.IO.Streams as Streams
-- import Data.ByteString.Lazy
import Data.String
-- import System.IO.Streams.Handle
-- import Debug.Trace ( trace )

class Monad m => HttpVersion m where
  -- | Are we running Http Version 1.1? Needed to send out the correct headers
  --   This needs to be implemented and depends on with web server you are using
  isHttpVersion1_1 :: m Bool

instance HttpVersion Identity  where
  isHttpVersion1_1 = return True

instance Default Text where
  def = ""

-- instance SSE Identity where
--   -- | Could be handy for debugging
--   sse x = Identity (trace (show x) ())

-- instance SSE IO where
--   -- | Could be handy for debugging
--   sse = Data.Text.IO.putStr 

-- class IsString b => Prompt a b | a -> b where
--   toPrompt :: a -> b

-- class ToText a where
--   toText :: a -> Text

-- instance ToText Text where
--   toText = id

-- instance ToText String where
--   toText = T.pack

-- instance  ToText Int where
--   toText = T.pack . show

-- instance  ToText Bool where
--   toText True  = "true"
--   toText False = "false"

-- instance ToText Builder where
--   toText = toText . show

-- class StringLike a -> Prompt a where
--   toPrompt 

type StringLike a = (Eq a, IsString a, Monoid a)

-- | I need a way to go from a Datastar type to a StringLike thing that can be
--   sent to the browser.  The Prompt class lets me do things like
--   prompt MergeFragments = "datastar-merge-fragments" without specifying
--   the exact type of the StringLike "datastar-merge-fragments"

class StringLike b => Prompt a b  where
  prompt :: a -> b
    
instance StringLike a => Prompt Bool a where
  prompt True  = "true"
  prompt False = "false"

instance StringLike a => Prompt Int a where
  prompt = fromString . show

instance Eq Builder where
  a == b = toLazyByteString a == toLazyByteString b
