module Day04.Grid
  ( Grid,
    Position,
    Direction,
    inBounds,
    countXMAS,
    findAllXMAS,
    directions,
    charAt,
    checkXMAS,
    readGrid,
    printGrid,
  )
where

import Control.Monad (forM_)

-- Types to represent the grid
type Grid = [[Char]]

type Position = (Int, Int)

type Direction = [Position]

-- Main function that counts all occurrences of XMAS
countXMAS :: Grid -> Int
countXMAS grid = length $ findAllXMAS grid

-- Find all positions where XMAS appears
findAllXMAS :: Grid -> [Position]
findAllXMAS grid =
  [ (row, col) | row <- [0 .. (length grid)], col <- [0 .. (length $ head grid)], direction <- directions, checkXMAS grid (row, col) direction
  ]

-- Helper function to get all possible directions for searching
directions :: [Direction]
directions =
  [ [(0, 1), (0, 2), (0, 3)],
    [(-1, 1), (-2, 2), (-3, 3)],
    [(-1, 0), (-2, 0), (-3, 0)],
    [(-1, -1), (-2, -2), (-3, -3)],
    [(0, -1), (0, -2), (0, -3)],
    [(1, -1), (2, -2), (3, -3)],
    [(1, 0), (2, 0), (3, 0)],
    [(1, 1), (2, 2), (3, 3)]
  ]

-- Helper to check if a position is within grid bounds
inBounds :: Grid -> Position -> Bool
inBounds [] _ = False -- Handle empty grid case
inBounds grid (row, col) =
  let height = length grid
      width = if height > 0 then length (head grid) else 0
   in row >= 0 && row < height && col >= 0 && col < width

-- Helper to get a character at a given position
charAt :: Grid -> Position -> Maybe Char
charAt grid position@(row, col) = if inBounds grid position then Just (grid !! row !! col) else Nothing

-- Helper to check if XMAS exists starting from a position in a given direction
checkXMAS :: Grid -> Position -> Direction -> Bool
checkXMAS grid start@(x0, y0) direction = charAt grid start == Just 'X' && [Just 'M', Just 'A', Just 'S'] == map f direction
  where
    f (dx, dy) = charAt grid (x0 + dx, y0 + dy)

-- Read input file and convert to grid
readGrid :: FilePath -> IO Grid
readGrid filepath = do
  contents <- readFile filepath
  -- Split into lines and return as grid
  return $ lines contents

-- Function to print grid (helpful for debugging)
printGrid :: Grid -> IO ()
printGrid grid = do
  forM_ grid $ \row -> do
    putStrLn row
