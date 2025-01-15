module Main where

import Test.HUnit
import Day04.Grid (Grid, Position, inBounds, charAt)

testGrid :: Grid
testGrid = [ "ABC"
          , "DEF"
          , "GHI"
          ]

emptyGrid :: Grid
emptyGrid = []

tests :: Test
tests = TestList [inBoundsTests, charAtTests]

inBoundsTests :: Test
inBoundsTests = TestList
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

charAtTests :: Test
charAtTests = TestList
  [ TestLabel "Valid position middle" $ TestCase $ do
      assertEqual "Should get 'E' at position (1,1)"
        (Just 'E') (charAt testGrid (1,1))

  , TestLabel "Valid position top-left" $ TestCase $ do
      assertEqual "Should get 'A' at position (0,0)"
        (Just 'A') (charAt testGrid (0,0))

  , TestLabel "Valid position bottom-right" $ TestCase $ do
      assertEqual "Should get 'I' at position (2,2)"
        (Just 'I') (charAt testGrid (2,2))

  , TestLabel "Out of bounds negative" $ TestCase $ do
      assertEqual "Should get Nothing for negative position"
        Nothing (charAt testGrid (-1,0))

  , TestLabel "Out of bounds too large" $ TestCase $ do
      assertEqual "Should get Nothing for position beyond grid"
        Nothing (charAt testGrid (3,3))

  , TestLabel "Empty grid" $ TestCase $ do
      assertEqual "Should get Nothing for empty grid"
        Nothing (charAt emptyGrid (0,0))
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts > 0
    then error "Tests failed!"
    else putStrLn "All tests passed!"
