import Test.HUnit
import Lib
import Data.Text (Text)
import qualified Data.Text as T

exampleRules :: Text
exampleRules = T.unlines
    [ "47|53"
    , "97|13"
    , "97|61"
    , "97|47"
    , "75|29"
    , "61|13"
    , "75|53"
    , "29|13"
    , "97|29"
    , "53|29"
    , "61|53"
    , "97|53"
    , "61|29"
    , "47|13"
    , "75|47"
    , "97|75"
    , "47|61"
    , "75|61"
    , "47|29"
    , "75|13"
    , "53|13"
    ]

exampleUpdates :: Text
exampleUpdates = T.unlines
    [ "75,47,61,53,29"
    , "97,61,53,29,13"
    , "75,29,13"
    , "75,97,47,61,53"
    , "61,13,29"
    , "97,13,75,29,47"
    ]

testParseRule :: Test
testParseRule = TestList
    [ "parse valid rule" ~: 
        parseRule "47|53" ~?= Right (Rule 47 53)
    , "parse rule with spaces" ~:
        parseRule "47 | 53" ~?= Right (Rule 47 53)
    ]

testParseRules :: Test
testParseRules = TestList
    [ "parse multiple rules" ~:
        case parseRules "47|53\n97|13\n" of
            Right rules -> rules ~?= [Rule 47 53, Rule 97 13]
            Left err -> assertFailure $ "Failed to parse rules: " ++ show err
    ]

testParseUpdate :: Test
testParseUpdate = TestList
    [ "parse valid update" ~:
        parseUpdate "75,47,61,53,29" ~?= Right [75,47,61,53,29]
    , "parse update with spaces" ~:
        parseUpdate "75, 47, 61, 53, 29" ~?= Right [75,47,61,53,29]
    ]

testParseUpdates :: Test
testParseUpdates = TestList
    [ "parse multiple updates" ~:
        case parseUpdates "75,47,61,53,29\n97,61,53,29,13\n" of
            Right updates -> updates ~?= [[75,47,61,53,29], [97,61,53,29,13]]
            Left err -> assertFailure $ "Failed to parse updates: " ++ show err
    ]

testIsValidUpdate :: Test
testIsValidUpdate = TestCase $ do
    let Right rules = parseRules exampleRules
    -- Test cases from example
    assertBool "first update should be valid" $ 
        isValidUpdate rules [75,47,61,53,29]
    assertBool "second update should be valid" $
        isValidUpdate rules [97,61,53,29,13]
    assertBool "third update should be valid" $
        isValidUpdate rules [75,29,13]
    assertBool "fourth update should be invalid" $
        not $ isValidUpdate rules [75,97,47,61,53]
    assertBool "fifth update should be invalid" $
        not $ isValidUpdate rules [61,13,29]
    assertBool "sixth update should be invalid" $
        not $ isValidUpdate rules [97,13,75,29,47]

testFindMiddleNumber :: Test
testFindMiddleNumber = TestList
    [ "middle of odd length list" ~:
        findMiddleNumber [75,47,61,53,29] ~?= 61
    , "middle of another odd length list" ~:
        findMiddleNumber [97,61,53,29,13] ~?= 53
    , "middle of short odd length list" ~:
        findMiddleNumber [75,29,13] ~?= 29
    ]

testSolve :: Test
testSolve = TestCase $ do
    case solve exampleRules exampleUpdates of
        Right result -> assertEqual "sum of middle numbers" 143 result
        Left err -> assertFailure $ "Failed to solve: " ++ err

tests :: Test
tests = TestList
    [ TestLabel "Parse Rule" testParseRule
    , TestLabel "Parse Rules" testParseRules
    , TestLabel "Parse Update" testParseUpdate
    , TestLabel "Parse Updates" testParseUpdates
    , TestLabel "Valid Update" testIsValidUpdate
    , TestLabel "Find Middle Number" testFindMiddleNumber
    , TestLabel "Solve" testSolve
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then putStrLn "All tests passed!"
        else putStrLn "Some tests failed."
