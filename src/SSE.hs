module SSE (
    EventType(..)
  , Options(..)
  , DsString  -- ^ A type synonym for LazyByteString
  , makeSSE
  , sseHeaders
  ) where

import Constants
import Data.Default ( Default(..) )
import Data.String.Here.Interpolated ( i )
import Relude

-- | ServerSendEventData is the data type that datastar works with internally.
--
--      The DataLines field varies depending on the EventType.  Examples are:
--
-- *    data: fragments <div id="someId">with content</div>
-- *    data: signals {"show":true,"input":"Test","user":{"name":"","email":""}}
-- *    data: paths user.name
-- *    data: script window.location = "https://data-star.dev"
--
--      the Options vary, again depending on the EventType. Examples are:
--
-- *        - selector: #target
-- *        - useViewTransition: true
-- *        - onlyIfMissing: true
-- *        - retry: 300
-- *        - settleDuration: 200

data ServerSendEventData = ServerSendEventData {
    _sEventType     :: DsString
  , _sDataLines     :: [DsString]
  , _sOptions       ::  Maybe Options }
  deriving Show

-- | sseHeaders are the headers that must be sent to the browser in order to start sending
-- a server sent event stream

sseHeaders :: LByteString
sseHeaders = [i|HTTP/1.1 200 OK
cache-control: no-cache
connection: keep-alive
Content-type: text/event-stream; charset=utf-8|] <> "\n\n"

-- | Options is the datatype for all available options for the various datastar commands
--   Depending on the EventType, the options are:
--
--  *  EventId
--  *  Selector
--  *  MergeMode
--  *  RetryDuration
--  *  SettleDuration
--  *  UseViewTransition
--  *  OnlyIfMissing
--  *  AutoRemove
--  *  Attributes
--  *  All options fields are prefixed with a _o to keep the namespace separate
--

data Options = Options {
    _oEventId                   :: Maybe DsString
  , _oSelector                  :: Maybe DsString
  , _oMergeMode                 :: Maybe DsString
  , _oRetryDuration             :: Maybe Int
  , _oSettleDuration            :: Maybe Int
  , _oUseViewTransition         :: Maybe Bool
  , _oOnlyIfMissing             :: Maybe Bool
  , _oAutoRemove                :: Maybe Bool
  , _oAttributes                :: Maybe DsString }
  deriving Show

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | EventType is the datatype of the datastar events.
--   The EventType is required and wiil be one of the haskell data contructors:
--
--       *  MergeFragments            --> "datastar-merge-fragments"
--       *  RemoveFragments           --> "datastar-remove-fragments"
--       *  MergeSignals              --> "datastar-merge-signals"
--       *  RemoveSignals             --> "datastar-remove-signals"
--       *  ExecuteScript             --> "datastar-execute-script"

data EventType =
    MergeFragments
  | RemoveFragments
  | MergeSignals
  | RemoveSignals
  | ExecuteScript
  deriving Show

-- | check returns an Either. It checks the arguments to be passed to makeSSE for
--   existance

check :: EventType -> [DsString] -> Maybe Options -> Either DsString (NonEmpty DsString)
check cmd [] mbOptions     = Left (showError "Empty List error" cmd mbOptions)
check cmd (x:xs) mbOptions = if x == mempty
  then Left  (showError"NoString error" cmd mbOptions)
  else Right (x :| xs)

-- | showError display a more user friendly error message if check returns a Left

showError :: DsString -> EventType -> Maybe Options -> DsString
showError err cmd mbOptions = mconcat [ err, " EventType: ", show cmd, " Options: ", show mbOptions]

newtype SSEexception = SSEexception DsString
  deriving Show
instance Exception SSEexception

-- | makeSSE is the main function you will use to create datastar events. It with throw an error
--   if the arguments are not correct, or a LazyByteString (internally called a DsString) if
--   the arguments check out.  This string may then be sent to the browser

makeSSE :: EventType -> [DsString] -> Maybe Options -> DsString
makeSSE cmd l mbOptions = either (bug . SSEexception) go (check cmd l mbOptions)
  where
   go n = case cmd of
    MergeFragments  ->   mergeFragments l mbOptions
    RemoveFragments ->   removeFragments (head n) mbOptions
    MergeSignals    ->   mergeSignals (head n) mbOptions
    RemoveSignals   ->   removeSignals l mbOptions
    ExecuteScript   ->   executeScript (head n) mbOptions

-- I need the class to lower case the show instance of Bools, and remove
-- the show instance of Nothings

class Show a => DsShow a where
  dsShow :: a -> DsString
  dsShow = show

instance DsShow Int  where
instance DsShow DsString where
  dsShow = id
instance DsShow Bool where
  dsShow True  = "true"
  dsShow False = "false"

instance DsShow a => DsShow (Maybe a) where
  dsShow Nothing  = ""
  dsShow (Just a) = dsShow a

-- A helper function to correctly format the various Options that are available

formatOptions :: Maybe Options -> DsString
formatOptions = maybe "" go
  where
    go o =
      mconcat [
       dsWithDefault o _oEventId           cDefaultEventId               cEventId
     , dsWithDefault o _oSelector          cDefaultSelector              cSelector
     , dsWithDefault o _oMergeMode         cDefaultMergeMode             cMerge
     , dsWithDefault o _oRetryDuration     cDefaultSseRetryDurationMs    cRetryDuration
     , dsWithDefault o _oSettleDuration    cDefaultSettleDurationMs      cSettleDuration
     , dsWithDefault o _oUseViewTransition cDefaultUseViewTransition     cUseViewTransition
     , dsWithDefault o _oOnlyIfMissing     cDefaultOnlyIfMissing         cOnlyIfMissing
     , dsWithDefault o _oAutoRemove        cDefaultAutoRemove            cAutoRemove
     , dsWithDefault o _oAttributes        cDefaultAttributes            cAttributes
     ]

-- The rest of these functions are used internally to do the formatting

withSpace :: DsString -> DsString -> DsString
withSpace x y = mconcat [ x, " ", y]

withNewLine :: DsString -> DsString -> DsString
withNewLine x y = mconcat [ x, " ", y, "\n"]

dsWithDefault :: (Eq a, DsShow a) => Options -> (Options -> Maybe a) -> a -> DsString -> DsString
dsWithDefault opts field defValue subCommand =
  case field opts of
    Nothing -> ""
    Just x -> if x == defValue then "" else mconcat [subCommand, ": ", dsShow x, "\n"]

formatSSED :: ServerSendEventData -> DsString
formatSSED ssed = mconcat [
  withNewLine  "event:" (_sEventType ssed),
  formatOptions (_sOptions ssed),
  mconcat (map (withNewLine "data:")  (_sDataLines ssed)),
  "\n" ]

mergeFragments :: [DsString] -> Maybe Options -> DsString
mergeFragments dsStrList mbOptions = formatSSED $
  ServerSendEventData cMergeFragments (map (withSpace cFragments) dsStrList) mbOptions

removeFragments :: DsString -> Maybe Options -> DsString
removeFragments dsStr mbOptions = formatSSED $
  ServerSendEventData cRemoveFragments [] (Just options)
  where
    options =  (fromMaybe def mbOptions) {_oSelector = Just dsStr}

mergeSignals :: DsString -> Maybe Options -> DsString
mergeSignals dsStr mbOptions = formatSSED $
  ServerSendEventData cMergeSignals [withSpace cSignals dsStr] mbOptions

removeSignals :: [DsString] -> Maybe Options -> DsString
removeSignals  dsStrList mbOptions = formatSSED $
  ServerSendEventData cRemoveSignals (map (withSpace cPaths) dsStrList) mbOptions

executeScript :: DsString -> Maybe Options -> DsString
executeScript dsStr mbOptions = formatSSED $
  ServerSendEventData cExecuteScript [withSpace cScript dsStr] mbOptions

-- see tests for an example
