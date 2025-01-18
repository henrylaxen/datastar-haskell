module Newtypes where

import NeatInterpolation
import qualified Data.Text as T
import Data.Default
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder
-- import Data.List.Extra

-- I just don't have the time or inclination to learn Template Haskell right now


newtypeStrings :: [[String]]
newtypeStrings = [
 ["SettleDuration"    ,"Int"          , "cDefaultSettleDurationMs"]
 , ["UseViewTransition" ,"Bool"       , "cDefaultUseViewTransition"]
 , ["EventId"           ,"SB"         , "mempty"]
 , ["RetryDuration"     ,"Int"        , "cDefaultSseRetryDurationMs"]
 , ["DataLines"         ,"[SB]"       , "[]"]
 , ["Selector"          ,"SB"         , "mempty"]
 , ["OnlyIfMissing"     ,"Bool"       , "cDefaultOnlyIfMissing"]
 , ["SignalsPath"       ,"[SB]"       , "[]"]
 , ["Script"            ,"SB"         , "mempty"]
 , ["Attributes"        ,"[SB]"       , "[SB cDefaultAttributes]"]
 , ["AutoRemove"        ,"Bool"       , "cDefaultAutoRemove"] ]
 
splitEvery :: Int -> String -> [String]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

trim :: String -> String
trim = trimStart . trimEnd
  where
    trimStart = dropWhile (== ' ')
    trimEnd = reverse . trimStart . reverse

template :: [T.Text] -> String
template [a,b,c] = T.unpack [untrimming|
newtype $a = $a $b
  deriving (Eq, Show, ToBuilder)
instance Default $a
  where def = $a $c
|]
template x = error (show x)

literally :: String
literally = T.unpack [untrimming|
newtype SB = SB Builder
  deriving (Monoid, Semigroup, Show)
instance Default SB
  where def = SB mempty

instance Eq SB where
 SB a == SB b = show a == show b

instance ToBuilder SB where
  toBuilder (SB x) = x

|]

makeNewtypes :: [String]
makeNewtypes =
  let
    s = map make1 newtypeStrings
    make1 = template . map T.pack
  in s

main :: IO ()
main = do
  putStr literally
  mapM_ putStr makeNewtypes

newtype SB = SB Builder
  deriving (Monoid, Semigroup, Show)
instance Default SB
  where def = SB mempty

instance Eq SB where
 SB a == SB b = show a == show b

instance ToBuilder SB where
  toBuilder (SB x) = x


newtype SettleDuration = SettleDuration Int
  deriving (Eq, Show, ToBuilder)
instance Default SettleDuration
  where def = SettleDuration cDefaultSettleDurationMs

newtype UseViewTransition = UseViewTransition Bool
  deriving (Eq, Show, ToBuilder)
instance Default UseViewTransition
  where def = UseViewTransition cDefaultUseViewTransition

newtype EventId = EventId SB
  deriving (Eq, Show, ToBuilder)
instance Default EventId
  where def = EventId mempty

newtype RetryDuration = RetryDuration Int
  deriving (Eq, Show, ToBuilder)
instance Default RetryDuration
  where def = RetryDuration cDefaultSseRetryDurationMs

newtype DataLines = DataLines [SB]
  deriving (Eq, Show, ToBuilder)
instance Default DataLines
  where def = DataLines []

newtype Selector = Selector SB
  deriving (Eq, Show, ToBuilder)
instance Default Selector
  where def = Selector mempty

newtype OnlyIfMissing = OnlyIfMissing Bool
  deriving (Eq, Show, ToBuilder)
instance Default OnlyIfMissing
  where def = OnlyIfMissing cDefaultOnlyIfMissing

newtype SignalsPath = SignalsPath [SB]
  deriving (Eq, Show, ToBuilder)
instance Default SignalsPath
  where def = SignalsPath []

newtype Script = Script SB
  deriving (Eq, Show, ToBuilder)
instance Default Script
  where def = Script mempty

newtype Attributes = Attributes [SB]
  deriving (Eq, Show, ToBuilder)
instance Default Attributes
  where def = Attributes [SB cDefaultAttributes]

newtype AutoRemove = AutoRemove Bool
  deriving (Eq, Show, ToBuilder)
instance Default AutoRemove
  where def = AutoRemove cDefaultAutoRemove
