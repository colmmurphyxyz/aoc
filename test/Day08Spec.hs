module Day08Spec where

import Test.Hspec

import Day08 (part1, part2)
import Util (getInputLines)


spec :: Spec
spec = do
    describe "Day08 - part1" $ do
        it "..." $ do
            input <- getInputLines "data/day08-input.txt"
            part1 input `shouldBe` 269
    describe "Day08 - part2" $ do
        it "..." $ do
            input <- getInputLines "data/day07-input.txt"
            part2 input `shouldBe` 949