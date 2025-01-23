{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Class where

import           Data.ByteString.Builder
import           Data.ByteString.Lazy.UTF8
import           Data.Default
import           Data.Functor.Identity     ( Identity(..) )
import           Data.Text                 ( Text )
import qualified Data.Text.Encoding        as T ( encodeUtf8 )
import           Debug.Trace
import           System.IO

class Monad m => HttpVersion m where
  -- | Are we running Http Version 1.1? Needed to send out the correct headers
  --   This needs to be implemented and depends on with web server you are using
  isHttpVersion1_1 :: m Bool

instance HttpVersion Identity  where
  isHttpVersion1_1 = return True

instance Eq Builder where
  a == b = show a == show b

instance Default Builder where
  def = mempty

class ToBuilder a where
  -- | Function to allow you to use types other than Builder
  --   to do stuff. Better if you just stick with Builder.
  toBuilder :: a -> Builder

instance ToBuilder Builder where
  toBuilder = id

instance ToBuilder ByteString where
  toBuilder = lazyByteString

instance ToBuilder Text where
  toBuilder = byteString . T.encodeUtf8

instance ToBuilder Char where
  toBuilder = Data.ByteString.Builder.char8

instance ToBuilder Int where
  toBuilder =  intDec

instance ToBuilder a => ToBuilder [a] where
  toBuilder =  mconcat . map toBuilder

instance ToBuilder Bool where
  toBuilder False = "false"
  toBuilder True  = "true"

class Monad m => SSE m where
  -- | Function to do the actual IO.  Dependent
  --   on which web server you are using
  sse :: Builder -> m ()

instance SSE Identity where
  -- | Could be handy for debugging
  sse x = Identity (trace (show x) ())

instance SSE IO where
  -- | Could be handy for debugging
  sse = hPutBuilder stdout

-- | A handy little helper to watch the result of sending stuff through sse
watch ::  Builder -> ()
watch x = runIdentity (sse x)

-- class Monad m => ReadSignals m where
--   r ::
