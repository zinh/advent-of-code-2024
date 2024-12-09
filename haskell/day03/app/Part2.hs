module Main where
import System.Environment (getArgs)
import Lib (calculate)

main :: IO ()
main = do
  (filename : _) <- getArgs
  content <- readFile filename
  print (calculate content)
