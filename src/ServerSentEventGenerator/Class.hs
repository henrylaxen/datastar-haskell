{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Class where

import Data.Default ( Default(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Text ( pack, Text )
import Data.Text.IO ( putStr )
import Debug.Trace ( trace )

class Monad m => HttpVersion m where
  -- | Are we running Http Version 1.1? Needed to send out the correct headers
  --   This needs to be implemented and depends on with web server you are using
  isHttpVersion1_1 :: m Bool

instance HttpVersion Identity  where
  isHttpVersion1_1 = return True

instance Default Text where
  def = ""

class Monad m => SSE m where
  -- | Function to do the actual IO.  Dependent
  --   on which web server you are using
  sse :: Text -> m ()

instance SSE Identity where
  -- | Could be handy for debugging
  sse x = Identity (trace (show x) ())

instance SSE IO where
  -- | Could be handy for debugging
  sse = Data.Text.IO.putStr

class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id

instance  ToText Int where
  toText = pack . show

instance  ToText Bool where
  toText True  = "true"
  toText False = "false"


