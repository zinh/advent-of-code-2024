module Main where

import Data.List.Split (splitOn)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  content <- readFile filename
  let r = length $ filter id $ map (valid . parseLine) (lines content)
   in print r

parseLine :: String -> [Integer]
parseLine line = map read (splitOn " " line)

valid :: [Integer] -> Bool
valid = foldl (\memo n -> False) False
