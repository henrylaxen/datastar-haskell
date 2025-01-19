module GenerateNewtypes where

import GHC.Base ()
import Data.String (IsString)
import NeatInterpolation
import qualified Data.Text as T

-- I just don't have the time or inclination to learn Template Haskell right now

-- ["The Symbol Name", "It's Inner type", "Name of default", "Name of prefix"]

newtypeStrings :: [[String]]
newtypeStrings = [
    ["SettleDuration","Int","cDefaultSettleDurationMs",""]
  , ["UseViewTransition","Bool","",""]
  , ["EventId","Builder","","cEventId"]
  , ["RetryDuration","Int","cDefaultSseRetryDurationMs","cRetryDuration"]
  , ["DataLines","[Builder]","mempty","cData"]
  , ["Selector","Builder","",""]
  , ["OnlyIfMissing","Bool","",""]
  , ["Signals","Builder","mempty",""]
  , ["SignalsPath","[Builder]","mempty","cPaths"]
  , ["Script","Builder","mempty",""]
  , ["Attributes","[Builder]","[cDefaultAttributes]",""]
  , ["AutoRemove","Bool","",""]
--  , ["MergeMode","Bool","",""]
--  , ["SignalSelector","Bool","",""]
  ]

template :: [T.Text] -> String
template [a,b,c,d] = result
  where
    defString = if T.null c then "cDefault" <> a else c
    result = T.unpack [untrimming|
newtype $a = $a $b
  deriving (Eq, Show, ToBuilder)
instance Default $a
  where def = $a $defString
instance DsCommand $a where
  dsCommand _ = $d|]


template x = error (show x)

literally :: String
literally = T.unpack [untrimming|
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerSentEventGenerator.Newtypes where
import Data.Default
import ServerSentEventGenerator.Class
import ServerSentEventGenerator.Constants
import Data.ByteString.Builder
|]

--  dsCommand = if T.null d then "c" <> a else d

fixup :: (Eq a, Data.String.IsString a, Semigroup a) => [a] -> [a]
fixup [a,b,c,d]
  | a == "EventId" || a == "RetryDuration" || a ==  "DataLines" = [a,b,c, d <> " <> \": \""]
  | otherwise = [a,b,c,"cData <> \": \" <> "  <> (if d == "" then "c" <> a else d) ]
fixup _ = error "fixup expects a list with four elements "

make1 :: [String] -> String
make1 = template . map T.pack

outputFile :: FilePath
outputFile = "src/ServerSentEventGenerator/Newtypes.hs"

makeNewtypes :: [String]
makeNewtypes = map (make1 . fixup) newtypeStrings

main :: IO ()
main = do
  let output = literally : makeNewtypes
  writeFile outputFile (concat output)
