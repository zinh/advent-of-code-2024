-- test/Main.hs
module Main where

import Test.Hspec (hspec)
import GuardPathSpec (spec)

main :: IO ()
main = hspec spec
