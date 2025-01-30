module GuardPathSpec (spec) where  -- Export the `spec` function

import Test.Hspec (Spec, describe, it, shouldBe)
import GuardPath (parseInput, countDistinctPositions, Position(..), GuardState(..), Direction(..))
import Data.Text (pack)
import Data.Map (Map)
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "parseInput" $ do
    it "parses a simple grid with a guard" $ do
      let input = pack "....#.....\n....^....#\n..........\n"
      let expectedGrid = Map.fromList [ (Position 0 0, False), (Position 1 0, False), (Position 2 0, False), (Position 3 0, False), (Position 4 0, True)
                                      , (Position 5 0, False), (Position 6 0, False), (Position 7 0, False), (Position 8 0, False), (Position 9 0, False)
                                      , (Position 0 1, False), (Position 1 1, False), (Position 2 1, False), (Position 3 1, False), (Position 4 1, False)
                                      , (Position 5 1, False), (Position 6 1, False), (Position 7 1, False), (Position 8 1, True), (Position 0 2, False)
                                      , (Position 1 2, False), (Position 2 2, False), (Position 3 2, False), (Position 4 2, False), (Position 5 2, False)
                                      , (Position 6 2, False), (Position 7 2, False), (Position 8 2, False), (Position 9 2, False) ]
      let expectedState = GuardState (Position 4 1) North
      parseInput input `shouldBe` Right (expectedGrid, expectedState)

  describe "countDistinctPositions" $ do
    it "counts distinct positions visited by the guard" $ do
      let grid = Map.fromList [ (Position 0 0, False), (Position 1 0, False), (Position 2 0, False), (Position 3 0, False), (Position 4 0, True)
                              , (Position 5 0, False), (Position 6 0, False), (Position 7 0, False), (Position 8 0, False), (Position 9 0, False)
                              , (Position 0 1, False), (Position 1 1, False), (Position 2 1, False), (Position 3 1, False), (Position 4 1, False)
                              , (Position 5 1, False), (Position 6 1, False), (Position 7 1, False), (Position 8 1, True), (Position 0 2, False)
                              , (Position 1 2, False), (Position 2 2, False), (Position 3 2, False), (Position 4 2, False), (Position 5 2, False)
                              , (Position 6 2, False), (Position 7 2, False), (Position 8 2, False), (Position 9 2, False) ]
      let state = GuardState (Position 4 1) North
      countDistinctPositions grid state `shouldBe` 41
