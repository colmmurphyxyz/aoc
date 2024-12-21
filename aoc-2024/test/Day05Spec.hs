module Day05Spec where

import Test.Hspec

import Day05(part1, part2)
import Util (getInput)

spec :: Spec
spec = do
    describe "Day05 - part1" $ do
        it "..." $ do
            input <- getInput "data/day05-input.txt"
            part1 input `shouldBe` 5452
    describe "Day05 - part2" $ do
        it "..." $ do
            input <- getInput "data/day05-input.txt"
            part2 input `shouldBe` 4598

