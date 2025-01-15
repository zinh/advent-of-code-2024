module Lib
    ( Rule(..)
    , Update
    , parseRule
    , parseRules
    , parseUpdate
    , parseUpdates
    , isValidUpdate
    , findMiddleNumber
    , solve
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Set as Set

-- | Represents a rule where first page must come before second page
data Rule = Rule
    { before :: Int  -- ^ Page that must come before
    , after  :: Int  -- ^ Page that must come after
    } deriving (Show, Eq)

-- | Represents an update as a list of page numbers
type Update = [Int]

-- | Parse a single rule in format "X|Y"
parseRule :: Text -> Either ParseError Rule
parseRule = undefined  -- TODO: Implement

-- | Parse multiple rules, one per line
parseRules :: Text -> Either ParseError [Rule]
parseRules = undefined  -- TODO: Implement

-- | Parse a single update (comma-separated numbers)
parseUpdate :: Text -> Either ParseError Update
parseUpdate = undefined  -- TODO: Implement

-- | Parse multiple updates, one per line
parseUpdates :: Text -> Either ParseError [Update]
parseUpdates = undefined  -- TODO: Implement

-- | Check if an update follows all applicable rules
-- Only considers rules where both pages are present in the update
isValidUpdate :: [Rule] -> Update -> Bool
isValidUpdate = undefined  -- TODO: Implement

-- | Find the middle number in an update
-- For odd length updates, returns the middle number
-- For even length updates, returns the lower middle number
findMiddleNumber :: Update -> Int
findMiddleNumber = undefined  -- TODO: Implement

-- | Solve the puzzle:
-- 1. Parse rules and updates
-- 2. Find valid updates
-- 3. Get middle numbers from valid updates
-- 4. Sum the middle numbers
solve :: Text -> Text -> Either String Int
solve = undefined  -- TODO: Implement
