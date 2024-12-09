module Lib where

import Text.Regex.TDFA
import Data.List.Split (splitOn)
import Debug.Trace (trace)

calcLine :: String -> Integer
calcLine content =
  let matches = (content =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]])
   in sum [calc match | match <- matches]

calc :: [String] -> Integer
calc (_ : a : b : _) = read a * read b

calculate :: String -> Integer
calculate content =
  let doLines = splitOn "do()" content
      lines = [head (splitOn "don't()" doLine) | doLine <- doLines]
  in sum [calcLine line | line <- lines]
