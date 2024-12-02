module Day01Spec where

import Prelude
import Test.Hspec

import Day01 (part1, part2, parseInput)

getInput :: IO [(Int, Int)]
getInput = do
  inputLines <- lines <$> readFile "data/day01-input.txt"
  return $ parseInput inputLines

spec :: Spec
spec = do
  describe "Day 01 - part1" $ do
    it "..." $ do
      input <- getInput
      part1 input `shouldBe` 1590491
  describe "Day 01 - part2" $ do
    it "..." $ do
      input <- getInput
      part2 input `shouldBe` 22588371
