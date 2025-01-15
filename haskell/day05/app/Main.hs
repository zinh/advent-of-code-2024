module Main (main) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [rulesFile, updatesFile] -> do
            rules <- TIO.readFile rulesFile
            updates <- TIO.readFile updatesFile
            case solve rules updates of
                Right result -> print result
                Left err -> putStrLn $ "Error: " ++ err
        _ -> putStrLn "Usage: day05 rules.txt updates.txt"
