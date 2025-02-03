{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

-- import Control.Applicative ( Alternative((<|>)) )
-- import Control.Concurrent ( threadDelay )
-- import Control.Monad ( foldM_ )
-- import Control.Monad.IO.Class ( MonadIO(liftIO) )
-- import Data.ByteString.Lazy
-- import Data.Maybe ( fromMaybe )
-- import Data.Time ( getCurrentTime )
import ServerSentEventGenerator.Types
import ServerSentEventGenerator.Constants
-- import Snap
-- import System.IO
--     ( stdout, hSetBuffering, stderr, BufferMode(NoBuffering) )
-- import qualified Data.Text as T
-- import Data.Aeson.Types hiding ( Options )
-- import Data.Aeson.KeyMap
-- import qualified Data.Aeson.Key as Key
-- import qualified Data.Vector as V
import NeatInterpolation
import Data.Aeson hiding ( Options )
import Data.Default ( Default(def) )
import Data.Text
import ServerSentEventGenerator
import Data.String

import Text.Show.Pretty (pPrint)
  
main :: IO ()
main = do
  putStrLn "all done"

js1 :: Text
js1 = [untrimming|{"events":
  [
    { "type": "executeScript",
      "script": "console.log('hello');",
      "eventId": 1,
      "retryDuration": 2000,
       "attributes": {
         "type": "text/javascript",
         "blocking": false
       },
       "autoRemove": false
     }
   ]
}
|]

js2 :: Text
js2 = [untrimming|
{
  "events": [
    {
      "type": "mergeFragments",
      "fragments": "<div>Merge</div>",
      "eventId": "event1",
      "retryDuration": 2000,
      "selector": "div",
      "mergeMode": "append",
      "settleDuration": 1000,
      "useViewTransition": true
    }
  ]
}
|]

js3 :: Text
js3 = [untrimming|
{
  "events": [
    {
      "type": "mergeFragments",
      "fragments": "<div>Merge</div>"
    }
  ]
}
|]

  
  
-- Just t1 = decodeStrictText js1 :: Maybe Value
-- Just t2 = decodeStrictText js2 :: Maybe Value
-- Just t3 = decodeStrictText js3 :: Maybe Value

instance ToJSON Options where
  toJSON O{..} = object [
        cEventId .= eventId,
        cRetryDuration .= retryDuration
    ]

instance FromJSON Options where
  parseJSON = withObject "Options" $ \v -> O
        <$> v .:? cEventId .!= mempty
        <*> v .:? cRetryDuration .!= cDefaultSseRetryDurationMs

instance ToJSON Selector where
    toJSON (SEL x) = String x
    -- or with record syntax:
    -- toJSON = String . unSelector

instance FromJSON Selector where
    parseJSON = withText cSelector $ \t ->
        pure $ SEL t

-- t3a =  toJSON $ SEL "#idx"
-- t3b =  toJSON $ SEL ""
-- t3 = (t3a,t3b)

instance ToJSON EventType where
    toJSON MergeFragments         = String cMergeFragments
    toJSON RemoveFragments        = String cRemoveFragments
    toJSON MergeSignals           = String cMergeSignals
    toJSON RemoveSignals          = String cRemoveSignals
    toJSON ExecuteScript          = String cExecuteScript

instance FromJSON EventType where
    parseJSON (String s)
        | s == cMergeFragments    = pure MergeFragments
        | s == cRemoveFragments   = pure RemoveFragments
        | s == cMergeSignals      = pure MergeSignals
        | s == cRemoveSignals     = pure RemoveSignals
        | s == cExecuteScript     = pure ExecuteScript
        | otherwise               = fail $ "Invalid EventType: " ++ show s
    parseJSON invalid             = fail $ "Invalid EventType: " ++ show invalid

instance ToJSON MergeMode where
    toJSON Morph                  = String cMorph
    toJSON Inner                  = String cInner
    toJSON Outer                  = String cOuter
    toJSON Prepend                = String cPrepend
    toJSON Append                 = String cAppend
    toJSON Before                 = String cBefore
    toJSON After                  = String cAfter
    toJSON UpsertAttributes       = String cUpsertAttributes

instance FromJSON MergeMode where
    parseJSON (String s)
        | s == cMorph             = pure Morph
        | s == cInner             = pure Inner
        | s == cOuter             = pure Outer
        | s == cPrepend           = pure Prepend
        | s == cAppend            = pure Append
        | s == cBefore            = pure Before
        | s == cAfter             = pure After
        | s == cUpsertAttributes  = pure UpsertAttributes
        | otherwise               = fail $ "Invalid MergeMode: " ++ show s
    parseJSON invalid             = fail $ "Invalid MergeMode: " ++ show invalid

instance ToJSON FragmentOptions where
    toJSON FO{..} = object [
        cSettleDuration    .= settleDuration,
        cUseViewTransition .= useViewTransition
      ]

instance FromJSON FragmentOptions where
    parseJSON = withObject "FragmentOptions" $ \v -> FO
        <$> v .:? cSettleDuration    .!= settleDuration    def
        <*> v .:? cUseViewTransition .!= useViewTransition def

data JMergeFragments = JMergeFragments {
    jtype              :: Text
  , jfragments         :: Text
  , jeventId           :: Text
  , jretryDuration     :: Int
  , jselector          :: Text
  , jmergeMode         :: Text
  , jsettleDuration    :: Int
  , juseViewTransition :: Bool }
  deriving Show

instance ToJSON JMergeFragments where
    toJSON JMergeFragments{..} = object
        [ "type" .= jtype
        , "fragments" .= jfragments
        , "eventId" .= jeventId
        , "retryDuration" .= jretryDuration
        , "selector" .= jselector
        , "mergeMode" .= jmergeMode
        , "settleDuration" .= jsettleDuration
        , "useViewTransition" .= juseViewTransition
        ]

instance FromJSON JMergeFragments where
    parseJSON = withObject "JMergeFragments" $ \v -> JMergeFragments
        <$> v .: "type"
        <*> v .: "fragments"
        <*> v .:? "eventId" .!= ""
        <*> v .:? "retryDuration" .!= cDefaultSseRetryDurationMs
        <*> v .:? "selector" .!= ""
        <*> v .:? "mergeMode" .!= ""
        <*> v .:? "settleDuration" .!= cDefaultSettleDurationMs
        <*> v .:? "useViewTransition" .!= cDefaultUseViewTransition

data Events = Events {
  jevents :: [JMergeFragments ]
                     }
  deriving Show

instance ToJSON Events where
    toJSON Events{..} = object
        [ "events" .= jevents
        ]

instance FromJSON Events where
    parseJSON = withObject "Events" $ \v -> Events
        <$> v .: "events"

