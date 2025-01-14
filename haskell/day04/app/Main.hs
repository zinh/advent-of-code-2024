module Main where
import System.IO
import Control.Monad (forM_)

-- Types to represent the grid
type Grid = [[Char]]
type Position = (Int, Int)

-- Main function that counts all occurrences of XMAS
countXMAS :: Grid -> Int
countXMAS grid = length $ findAllXMAS grid

-- Find all positions where XMAS appears
findAllXMAS :: Grid -> [Position]
findAllXMAS grid = undefined  -- You'll implement this

-- Helper function to get all possible directions for searching
directions :: [(Int, Int)]
directions = [
    (-1,-1), (-1,0), (-1,1),  -- Upper left, up, upper right
    (0,-1),          (0,1),   -- Left, right
    (1,-1),  (1,0),  (1,1)    -- Lower left, down, lower right
    ]

-- Helper to check if a position is within grid bounds
inBounds :: Grid -> Position -> Bool
inBounds grid (row, col) = undefined  -- You'll implement this

-- Helper to get a character at a given position
charAt :: Grid -> Position -> Char
charAt grid (row, col) = undefined  -- You'll implement this

-- Helper to check if XMAS exists starting from a position in a given direction
checkXMAS :: Grid -> Position -> (Int, Int) -> Bool
checkXMAS grid start direction = undefined  -- You'll implement this

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

-- Example usage:
main :: IO ()
main = do
    grid <- readGrid "input.txt"
    let result = countXMAS grid
    putStrLn $ "XMAS appears " ++ show result ++ " times"

