module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let (left, m) = foldl parse ([], Map.empty) (map parseLine $ lines contents)
      r = sum $ map (\n -> Map.findWithDefault 0 n m * n) left
   in print (show r)

parseLine :: String -> (Integer, Integer)
parseLine line =
  let (left : rest) = splitOn " " line
      right = read $ last rest
   in (read left, right)

parse :: ([] Integer, Map.Map Integer Integer) -> (Integer, Integer) -> ([] Integer, Map.Map Integer Integer)
parse (lst, m) (left, right) = (left : lst, newM)
  where
    newM = case Map.lookup right m of
      Nothing -> Map.fromList [(right, 1)]
      Just v -> Map.insert right (v + 1) m
