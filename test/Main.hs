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

-- Just t1 = decodeStrictText js1 :: Maybe Value

-- data DSEvent = DSEvent {
--   eType :: EventType
--   eScript :: Text
--   eEventId :: Int
--   eRetryDuration :: Int
--   eAutoRemove :: Bool
--   eAttributes :: [Text] }

-- prependFailure :: String -> Parser a -> Parser a
-- data Coord = Coord { x :: Double, y :: Double }

-- instance FromJSON Coord where
--     parseJSON (Object v) = Coord
--         <$> v .: "x"
--         <*> v .: "y"

--     -- We do not expect a non-Object value here.
--     -- We could use empty to fail, but typeMismatch
--     -- gives a much more informative error message.
--     parseJSON invalid    =
--         prependFailure "parsing Coord failed, "
--             (typeMismatch "Object" invalid)
-- typeMismatch :: String -> Value -> Parser a
-- The name of the JSON type being parsed ("Object", "Array", "String", "Number", "Boolean", or "Null").
-- The actual value encountered.
-- unexpected :: Value -> Parser a

-- (Object (fromList [("events",Array
--   [Object (fromList [("attributes",Object (fromList [("blocking",Bool False),("type",String "text/javascript")])),("autoRemove",Bool False),("eventId",Number 1.0),("retryDuration",Number 2000.0),("script",String "console.log('hello');"),("type",String "executeScript")])])]))  

-- instance ToJSON Person where
--     -- this generates a Value
--     toJSON (Person name age) =
--         object ["name" .= name, "age" .= age]

--     -- this encodes directly to a bytestring Builder
--     toEncoding (Person name age) =
--         pairs ("name" .= name <> "age" .= age)

-- instance FromJSON ServerSentEventGenerator.Types.Options where
--   parseJSON (Object o) = O
--     <$> o .: "eventId"
--     <*> o .: "retryDuration"
--   parseJSON invalid = 
--     prependFailure "parsing Options failed, "
--       (typeMismatch "Object" invalid)


-- instance ToJSON ServerSentEventGenerator.Types.Options where
--   toJSON ( ServerSentEventGenerator.Types.O eventId retryDuration) =
--     object [Key.fromText cEventId .?= eventId, Key.fromText cRetryDuration .?= retryDuration]

-- t2a =  toJSON $ (O "ab123" 1001)
-- t2b =  toJSON $ (O "" 1001)
-- t2c =  toJSON $ (O "" 1000)
-- t2 = (t2a,t2b,t2c)       
       

-- instance ToJSON ServerSentEventGenerator.Types.Options where
--   toJSON ( ServerSentEventGenerator.Types.O eventId retryDuration) = object
--     [Key.fromText cEventId .= fromMaybe "" eventId]
    
--     object [Key.fromText cEventId .?= eventId, Key.fromText cRetryDuration .?= retryDuration]

instance ToJSON a => ToJSON (Options a) where
  toJSON O{..} = object [
        cEventId .= eventId,
        cRetryDuration .= retryDuration
    ]

instance (Monoid a, FromJSON a) => FromJSON (Options a) where
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
 
