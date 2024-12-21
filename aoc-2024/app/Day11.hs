module Day11 where

import Util (getInput)

parseInput :: String -> [Int]
parseInput = map read . words

numDigits :: Integral a => a -> Int
numDigits n
    | n < 10 = 1
    | otherwise = 1 + numDigits (n `div` 10)

split :: Int -> [Int]
split n =
    let
        half = 10 ^ (numDigits n `div` 2)
        lhs = n `div` half
        rhs = n `mod` half
    in
        [lhs, rhs]


transform :: [Int] -> [Int]
transform [] = []
transform (x:xs)
    | x == 0 = 1 : transform xs
    | even $ numDigits x = split x ++ transform xs
    | otherwise = (x * 2024) : transform xs

transformations :: [Int] -> [[Int]]
transformations = iterate transform

part1 :: String -> Int
part1 input = length $ transformations stones !! 25
    where
        stones = parseInput input

-- Will take a __very__ long time to run
part2 :: String -> Int
part2 input = length $ transformations stones !! 75
    where
        stones = parseInput input

day11 :: IO ()
day11 = do
    input <- getInput "data/day11-input.txt"
    print $ part1 input
    -- print $ part2 input

