module Main (main) where

import Data.Default ( Default(def) )
import Data.String.Here.Interpolated ( i )
import Relude
import SSE
    ( makeSSE,
      DsString,
      EventType(..),
      Options(_oAttributes, _oSettleDuration, _oUseViewTransition,
              _oEventId, _oOnlyIfMissing, _oRetryDuration, _oAutoRemove) )
import System.IO.Silently ( capture )

sseWrite :: DsString -> IO ()
sseWrite = putStr . decodeUtf8

commands :: [(EventType, [DsString], Maybe Options)]
commands = [
    ( MergeFragments,   [ [i|<div id="someId">with content</div>|]  ],  Nothing )
  , ( RemoveFragments,  ["#target"], Just (def {_oSettleDuration = Just 200, _oUseViewTransition = Just True}) )
  , ( MergeSignals,
    [ [i|{"output":"Patched Output Test","show":true,"input":"Test","user":{"name":"","email":""}}|]  ],
      Just (def {_oEventId = Just "123", _oOnlyIfMissing = Just True}) )
  , ( RemoveSignals,    [ "user.name", "user.email"], Just (def {_oRetryDuration = Just 300}) )
  , ( ExecuteScript,    [ [i|window.location = "https://data-star.dev"||]  ],
      Just (def {_oAutoRemove = Just False, _oAttributes = Just "type text/javascript"}))
  ]

testStdOutSend :: IO ()
testStdOutSend = mapM_ (\(a,b,c) -> sseWrite (makeSSE a b c)) commands

main :: IO ()
main = do
  (s,_) <- capture testStdOutSend
  if s == r1
    then putStrLn "test passed"
    else print s >> print r1

r1 :: String
r1 = [i|event: datastar-merge-fragments
data: fragments <div id="someId">with content</div>

event: datastar-remove-fragments
selector: #target
settleDuration: 200
useViewTransition: true

event: datastar-merge-signals
id: 123
onlyIfMissing: true
data: signals {"output":"Patched Output Test","show":true,"input":"Test","user":{"name":"","email":""}}

event: datastar-remove-signals
retry: 300
data: paths user.name
data: paths user.email

event: datastar-execute-script
autoRemove: false
attributes: type text/javascript
data: script window.location = "https://data-star.dev"|

|]
