{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Types where

import Control.Exception ( throw, Exception )
import Data.ByteString.Builder ( Builder )
import Data.Default ( Default(..) )
import Data.Text ( Text )
import ServerSentEventGenerator.Class ( ToText(..) )
import ServerSentEventGenerator.Constants
import ServerSentEventGenerator.Internal
    ( buildLines, withDefault )
import qualified System.IO.Streams as Streams ( OutputStream )
import Data.String

type SSEstream = Streams.OutputStream Builder
newtype SSEapp = SSEapp (SSEstream -> IO ())
type StringLike a = (Eq a, IsString a, Monoid a, ToText a)

data Options a = O {
    eventId       :: a
  , retryDuration :: Int
  } deriving (Show)

instance StringLike a => Default (Options a) where
  def = O {
    eventId = mempty
  , retryDuration = cDefaultSseRetryDurationMs
  }


instance StringLike a => ToText (Options a) where
  toText options =
    let
      withSSEdefault value defaultValue field =
        if value ==  defaultValue then mempty
           else field <> fromString ": " <> toText value
      a = withSSEdefault  (eventId options) mempty cEventId
      b = withSSEdefault  (retryDuration options) cDefaultSseRetryDurationMs cRetryDuration
    in mconcat . buildLines $ [a,b]

newtype Selector = SEL {unSelector :: Text}
  deriving (Show, Semigroup, Monoid, Eq)

instance Default Selector where
  def = SEL ""

instance ToText Selector where
  toText = withDefault cSelector cDefaultSelector . unSelector
    
-- | A sum of the possible Datastar specific sse events that can be sent

data EventType =
    MergeFragments
  | RemoveFragments
  | MergeSignals
  | RemoveSignals
  | ExecuteScript
  deriving (Eq, Show)

data FunctionExecuteScript a =  FunctionExecuteScript {
    eType       :: EventType
  , eScript     :: a
  , eAttributes :: Attributes a
  , eAutoRemove :: Bool
  , eOptions    :: Options a
  } deriving Show

-- instance Default (EventType a)
--   where def = MergeFragments a

instance ToText EventType where
  toText MergeFragments   = cMergeFragments
  toText RemoveFragments  = cRemoveFragments
  toText MergeSignals     = cMergeSignals
  toText RemoveSignals    = cRemoveSignals
  toText ExecuteScript    = cExecuteScript

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

instance ToText MergeMode where
   toText Morph            = cMorph
   toText Inner            = cInner
   toText Outer            = cOuter
   toText Prepend          = cPrepend
   toText Append           = cAppend
   toText Before           = cBefore
   toText After            = cAfter
   toText UpsertAttributes = cUpsertAttributes

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

data Attributes a = ATR {
    aType     :: a
  , aBlocking :: Bool
  } deriving (Show)


instance ToText FragmentOptions where
  toText (FO a b) = mconcat . buildLines $ [
      withDefault cSettleDuration    (toText cDefaultSettleDurationMs)   (toText a)
    , withDefault cUseViewTransition (toText cDefaultUseViewTransition)  (toText b)
    ]

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

