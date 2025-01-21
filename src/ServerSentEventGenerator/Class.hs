{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Class where

-- import Debug.Trace
import Data.Text ( Text )
import Data.ByteString.Lazy.UTF8
-- import System.IO
import qualified Data.Text.Encoding as T ( encodeUtf8 )
import Data.Default
import Data.ByteString.Builder
import Data.Functor.Identity ( Identity(..) )

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
  -- | Abstract function to allow you to use types other than Builder
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


-- class ToBuilders where
--   toBuilders :: a -> [Builder]

-- instance ToBuilder a => ToBuilders [a] where
--   toBuilders  = map toBuilder
{-





class Monad m => Sender m where
  -- | Abstract function to do the actual IO.  Again dependent
  --   on which web server you are using
  send :: Builder -> m ()

instance Sender Identity where
  send x = Identity (trace (show x) ())

instance Sender IO where
  send = hPutBuilder stdout

-- | A handy little helper to watch the result of sending stuff
watch :: Builder -> ()
watch x = runIdentity (send x) 

class ToBuilder a where
  -- | Abstract function to allow you to use types other than Builder
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

class DsCommand a where
  dsCommand :: a -> Builder
  

-}
