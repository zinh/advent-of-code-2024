module Main where

import Day04.Grid (Direction, Grid, Position, charAt, checkXMAS, inBounds)
import Test.HUnit

testGrid :: Grid
testGrid =
  [ "XMAS",
    "MAMX",
    "ASAM",
    "SMAS"
  ]

emptyGrid :: Grid
emptyGrid = []

tests :: Test
tests = TestList [inBoundsTests, charAtTests, checkXMASTests]

inBoundsTests :: Test
inBoundsTests =
  TestList
    [ TestLabel "Valid position" $ TestCase $ do
        assertEqual
          "Position (1,1) should be in bounds"
          True
          (inBounds testGrid (1, 1)),
      TestLabel "Top-left corner" $ TestCase $ do
        assertEqual
          "Position (0,0) should be in bounds"
          True
          (inBounds testGrid (0, 0)),
      TestLabel "Bottom-right corner" $ TestCase $ do
        assertEqual
          "Position (2,2) should be in bounds"
          True
          (inBounds testGrid (2, 2)),
      TestLabel "Negative row" $ TestCase $ do
        assertEqual
          "Position (-1,0) should be out of bounds"
          False
          (inBounds testGrid (-1, 0)),
      TestLabel "Negative column" $ TestCase $ do
        assertEqual
          "Position (0,-1) should be out of bounds"
          False
          (inBounds testGrid (0, -1)),
      TestLabel "Row too large" $ TestCase $ do
        assertEqual
          "Position (4,0) should be out of bounds"
          False
          (inBounds testGrid (4, 0)),
      TestLabel "Column too large" $ TestCase $ do
        assertEqual
          "Position (0,4) should be out of bounds"
          False
          (inBounds testGrid (0, 4)),
      TestLabel "Empty grid" $ TestCase $ do
        assertEqual
          "Any position in empty grid should be out of bounds"
          False
          (inBounds emptyGrid (0, 0))
    ]

charAtTests :: Test
charAtTests =
  TestList
    [ TestLabel "Valid position middle" $ TestCase $ do
        assertEqual
          "Should get 'M' at position (0,1)"
          (Just 'M')
          (charAt testGrid (0, 1)),
      TestLabel "Valid position top-left" $ TestCase $ do
        assertEqual
          "Should get 'X' at position (0,0)"
          (Just 'X')
          (charAt testGrid (0, 0)),
      TestLabel "Valid position bottom-right" $ TestCase $ do
        assertEqual
          "Should get 'S' at position (3,3)"
          (Just 'S')
          (charAt testGrid (3, 3)),
      TestLabel "Out of bounds negative" $ TestCase $ do
        assertEqual
          "Should get Nothing for negative position"
          Nothing
          (charAt testGrid (-1, 0)),
      TestLabel "Out of bounds too large" $ TestCase $ do
        assertEqual
          "Should get Nothing for position beyond grid"
          Nothing
          (charAt testGrid (4, 4)),
      TestLabel "Empty grid" $ TestCase $ do
        assertEqual
          "Should get Nothing for empty grid"
          Nothing
          (charAt emptyGrid (0, 0))
    ]

checkXMASTests :: Test
checkXMASTests =
  TestList
    [ TestLabel "Horizontal XMAS" $ TestCase $ do
        let direction = [(0, 1), (0, 2), (0, 3)]
        assertEqual
          "Should find XMAS horizontally"
          True
          (checkXMAS testGrid (0, 0) direction),
      TestLabel "Vertical XMAS" $ TestCase $ do
        let direction = [(1, 0), (2, 0), (3, 0)]
        assertEqual
          "Should find XMAS vertically"
          True
          (checkXMAS testGrid (0, 0) direction),
      TestLabel "Not XMAS (wrong start)" $ TestCase $ do
        let direction = [(0, 1), (0, 2), (0, 3)]
        assertEqual
          "Should not find XMAS when starting with wrong letter"
          False
          (checkXMAS testGrid (0, 1) direction),
      TestLabel "Not XMAS (incomplete pattern)" $ TestCase $ do
        let direction = [(0, 1), (0, 2), (0, 3)]
        assertEqual
          "Should not find XMAS when pattern is incomplete"
          False
          (checkXMAS testGrid (1, 0) direction),
      TestLabel "Out of bounds" $ TestCase $ do
        let direction = [(0, 1), (0, 2), (0, 3)]
        assertEqual
          "Should not find XMAS when pattern goes out of bounds"
          False
          (checkXMAS testGrid (3, 1) direction),
      TestLabel "Empty grid" $ TestCase $ do
        let direction = [(0, 1), (0, 2), (0, 3)]
        assertEqual
          "Should not find XMAS in empty grid"
          False
          (checkXMAS emptyGrid (0, 0) direction)
    ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts > 0
    then error "Tests failed!"
    else putStrLn "All tests passed!"
