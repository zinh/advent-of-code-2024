module GuardPath where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

-- Define the guard's position and direction
data Direction = North | East | South | West
  deriving (Eq, Show, Enum)

data Position = Position
  { posX :: Int
  , posY :: Int
  } deriving (Eq, Ord, Show)

data GuardState = GuardState
  { guardPos :: Position
  , guardDir :: Direction
  } deriving (Eq, Show)

-- Parse the map into a grid of positions and obstacles
type Grid = Map Position Bool

parseInput :: Text -> Either ParseError (Grid, GuardState)
parseInput = parse gridParser ""
  where
    gridParser = do
      rows <- sepEndBy1 (many1 cell) newline
      let grid = Map.fromList $ concat $ zipWith parseRow [0..] rows
      let guardState = findGuard grid
      return (grid, guardState)
    
    cell = (char '.' >> return False) <|> (char '#' >> return True) <|> (char '^' >> return False)
    
    parseRow y row = zipWith (\x isObstacle -> (Position x y, isObstacle)) [0..] row
    
    findGuard grid = GuardState (fst $ head $ filter (not . snd) $ Map.toList grid) North

-- Simulate the guard's path
simulateGuardPath :: Grid -> GuardState -> [Position]
simulateGuardPath grid state = undefined -- TODO: Implement this

-- Main function to count distinct positions
countDistinctPositions :: Grid -> GuardState -> Int
countDistinctPositions grid state = length $ simulateGuardPath grid state
