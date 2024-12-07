module Day07Spec where

import Test.Hspec

import Day07 (part1, part2, parseLine, Equation)
import Util (getInputLines)


spec :: Spec
spec = do
    describe "Day07 - part1" $ do
        it "..." $ do
            input <- map parseLine <$> getInputLines "data/day07-input.txt"
            part1 input `shouldBe` 8401132154762
    describe "Day07 - part2" $ do
        it "..." $ do
            input <- map parseLine <$> getInputLines "data/day07-input.txt"
            part2 input `shouldBe` 95297119227552
