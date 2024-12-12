module Sender.Snap where

import Data.ByteString.Builder ( lazyByteString, Builder )
import Data.ByteString.Builder.Extra ( flush )
import Relude
    ( ($), Num((*)), Ord(max), Maybe(Nothing, Just), MonadIO(..), IO )
import SSE ( DsString, sseHeaders )
import Snap ( escapeHttp, MonadSnap )
import qualified System.IO.Streams as Streams
    ( OutputStream, write )

type SSEstream = Streams.OutputStream Builder

sseOpen :: MonadSnap m => (SSEstream -> IO ()) -> m ()
sseOpen f = do
  escapeHttp $ \tickle _ writeEnd -> do
    tickle (max (60*60))
    Streams.write (Just (lazyByteString sseHeaders)) writeEnd
    f writeEnd

sseWrite :: MonadIO m => SSEstream -> DsString -> m ()
sseWrite writeEnd dsStr = liftIO $ do
  Streams.write (Just (lazyByteString dsStr)) writeEnd
  Streams.write (Just flush) writeEnd
sseClose :: MonadIO m => SSEstream -> m ()
sseClose writeEnd = liftIO $ Streams.write Nothing writeEnd

