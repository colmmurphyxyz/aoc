module Day02Spec where

import Prelude
import Test.Hspec

import Day02(part1, part2, parseInput)

getInput :: IO [[Int]]
getInput = do
    inputLines <- lines <$> readFile "data/day02-input.txt"
    return $ parseInput inputLines

spec :: Spec
spec = do
    describe "Day02 - part1" $ do
        it "..." $ do
            input <- getInput
            part1 input `shouldBe` 334
    describe "Day02 - part2" $ do
        it "..." $ do
            input <- getInput
            part2 input `shouldBe` 400
        