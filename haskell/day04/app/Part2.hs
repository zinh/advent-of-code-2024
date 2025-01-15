module Main where

import System.Environment (getArgs)
import Day04.Grid (readGrid, countXMAS2)

main :: IO ()
main = do
  (filename : _) <- getArgs
  grid <- readGrid filename
  let result = countXMAS2 grid
  putStrLn $ "XMAS appears " ++ show result ++ " times"
