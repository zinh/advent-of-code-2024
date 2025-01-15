module Main where

import System.Environment (getArgs)
import Day04.Grid (readGrid, countXMAS)

main :: IO ()
main = do
  (filename : _) <- getArgs
  grid <- readGrid filename
  let result = countXMAS grid
  putStrLn $ "XMAS appears " ++ show result ++ " times"
