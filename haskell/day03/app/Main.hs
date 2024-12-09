module Main where
import System.Environment (getArgs)
import qualified Lib

main :: IO ()
main = do
  (filename : _) <- getArgs
  content <- readFile filename
  print (Lib.calcLine content)
