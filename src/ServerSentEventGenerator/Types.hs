{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Types where

import Control.Exception ( throw, Exception )
import Data.ByteString.Builder ( Builder )
import Data.Default ( Default(..) )
import Data.Text ( Text, pack )
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Internal
    ( buildLines, withDefault )
import qualified System.IO.Streams as Streams ( OutputStream )
-- import Data.String

type SSEstream = Streams.OutputStream Builder
newtype SSEapp = SSEapp (SSEstream -> IO ())

data Options = O {
    eventId       :: Text
  , retryDuration :: Int
  } deriving (Show)

instance Default Options where
  def = O {
    eventId = mempty
  , retryDuration = cDefaultSseRetryDurationMs
  }

instance Prompt Options where
  prompt options =
    let
      a = eventId options
      b = if retryDuration options == cDefaultSseRetryDurationMs
        then "" else pack . show $ retryDuration options
    in mconcat . buildLines $ [a,b]

newtype Selector = SEL {unSelector :: Text}
  deriving (Show, Semigroup, Monoid, Eq)

instance Default Selector where
  def = SEL mempty

instance Prompt Selector where
  prompt (SEL x) = withDefault cSelector cDefaultSelector x
    
-- | A sum of the possible Datastar specific sse events that can be sent

data EventType =
    MergeFragments
  | RemoveFragments
  | MergeSignals
  | RemoveSignals
  | ExecuteScript
  deriving (Eq, Show)

instance Default EventType
  where def = MergeFragments

instance Prompt EventType where
  prompt MergeFragments   = cMergeFragments
  prompt RemoveFragments  = cRemoveFragments
  prompt MergeSignals     = cMergeSignals
  prompt RemoveSignals    = cRemoveSignals
  prompt ExecuteScript    = cExecuteScript


data FunctionExecuteScript =  FunctionExecuteScript {
    eType       :: EventType
  , eScript     :: Text
  , eAttributes :: Attributes
  , eAutoRemove :: Bool
  , eOptions    :: Options
  } deriving Show

-- | A sum of the possible Datastar specific merge modes that can be sent

data MergeMode =
     Morph
   | Inner
   | Outer
   | Prepend
   | Append
   | Before
   | After
   | UpsertAttributes
   deriving (Eq, Show)

instance Default MergeMode
  where def = Morph

instance Prompt MergeMode where
   prompt Morph            = cMorph
   prompt Inner            = cInner
   prompt Outer            = cOuter
   prompt Prepend          = cPrepend
   prompt Append           = cAppend
   prompt Before           = cBefore
   prompt After            = cAfter
   prompt UpsertAttributes = cUpsertAttributes

data FragmentOptions = FO {
    settleDuration    :: Int
  , useViewTransition :: Bool
  } deriving (Show)

-- | the MergeFragments and RemoveFragment data types share these options

instance Default FragmentOptions where
  def = FO {
    settleDuration     = cDefaultSettleDurationMs
  , useViewTransition  = cDefaultUseViewTransition
  }

instance Prompt FragmentOptions where
  prompt (FO a b) = mconcat . buildLines $ [
      withDefault cSettleDuration    (prompt cDefaultSettleDurationMs)   (prompt a)
    , withDefault cUseViewTransition (prompt cDefaultUseViewTransition)  (prompt b)
    ]

data Attributes = ATR {
    aType     :: Text
  , aBlocking :: Bool
  } deriving (Show)


instance Show ServerSentEventGeneratorExceptions where
 show BuildLineDataIMissing           = "buildLine was call with nothing to build"
 show RemoveFragmentSelectorIsMissing = "The selector field is required in RemoveFragment"
 show SignalsSelectorIsMissing        = "The selector field is required in MergeSignals"
 show RemoveSignalsPathIsMissing      = "The path field is required in RemoveSignals"
 show RemoveSignalsPathIsEmpty        = "The path field cannot be an empty list"
 show ExecuteScriptIsMissing          = "The script field is required in ExecuteScript"

bug :: Exception e => e -> a
bug = throw

data ServerSentEventGeneratorExceptions =
   BuildLineDataIMissing 
 | RemoveFragmentSelectorIsMissing 
 | SignalsSelectorIsMissing        
 | RemoveSignalsPathIsMissing      
 | RemoveSignalsPathIsEmpty        
 | ExecuteScriptIsMissing          

instance Exception ServerSentEventGeneratorExceptions

