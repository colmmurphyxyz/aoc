
module Day03Spec where

import Prelude
import Test.Hspec

import Day03(part1, part2)

spec :: Spec
spec = do
    describe "Day03 - part1" $ do
        it "..." $ do
            input <- readFile "data/day03-input.txt"
            part1 input `shouldBe` 183669043
    describe "Day03 - part2" $ do
        it "..." $ do
            input <- readFile "data/day03-input.txt"
            part2 input `shouldBe` 59097164
        