module Main where
import SnapMain
import WarpMain
import Relude

main :: IO ()
main = do
  putStrLn "Enter 1 for Snap, 2 for Warp, ^C to exit"
  chars <- getLine
  case chars of
    "1" -> snapMain
    "2" -> warpMain
    _   -> putStrLn "Please enter 1 or 2" >> main


  
