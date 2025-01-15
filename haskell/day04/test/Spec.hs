module Main where

import Test.HUnit
import Main (Grid, Position, inBounds)

testGrid :: Grid
testGrid = [ "ABC"
          , "DEF"
          , "GHI"
          ]

emptyGrid :: Grid
emptyGrid = []

tests :: Test
tests = TestList
  [ TestLabel "Valid position" $ TestCase $ do
      assertEqual "Position (1,1) should be in bounds"
        True (inBounds testGrid (1,1))

  , TestLabel "Top-left corner" $ TestCase $ do
      assertEqual "Position (0,0) should be in bounds"
        True (inBounds testGrid (0,0))

  , TestLabel "Bottom-right corner" $ TestCase $ do
      assertEqual "Position (2,2) should be in bounds"
        True (inBounds testGrid (2,2))

  , TestLabel "Negative row" $ TestCase $ do
      assertEqual "Position (-1,0) should be out of bounds"
        False (inBounds testGrid (-1,0))

  , TestLabel "Negative column" $ TestCase $ do
      assertEqual "Position (0,-1) should be out of bounds"
        False (inBounds testGrid (0,-1))

  , TestLabel "Row too large" $ TestCase $ do
      assertEqual "Position (3,0) should be out of bounds"
        False (inBounds testGrid (3,0))

  , TestLabel "Column too large" $ TestCase $ do
      assertEqual "Position (0,3) should be out of bounds"
        False (inBounds testGrid (0,3))

  , TestLabel "Empty grid" $ TestCase $ do
      assertEqual "Any position in empty grid should be out of bounds"
        False (inBounds emptyGrid (0,0))
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts > 0
    then error "Tests failed!"
    else putStrLn "All tests passed!"
