module Main where

import Data.List.Split (splitOn)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let r = sum [sumPair $ parseLine line | line <- lines contents]
   in print r

parseLine :: String -> (Integer, Integer)
parseLine line =
  let (first : rest) = splitOn " " line
   in (read first, read (last rest))

sumPair :: (Integer, Integer) -> Integer
sumPair (left, right) = abs (left - right)
