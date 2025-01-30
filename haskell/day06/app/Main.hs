module Main where

import GuardPath
import Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  input <- TIO.readFile "input.txt"
  case parseInput input of
    Left err -> print err
    Right (grid, state) -> print $ countDistinctPositions grid state
