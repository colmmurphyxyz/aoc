{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day04 (day04) where

import Data.List.Split (splitOn)

isSixDigits :: Int -> Bool
isSixDigits n = n >= 100000 && n < 1000000

hasAdjacentDigits :: String -> Bool
hasAdjacentDigits [] = False
hasAdjacentDigits [_] = False
hasAdjacentDigits (x:y:xs) = x == y || hasAdjacentDigits (y:xs)

groupEquals :: Eq a => [a] -> [[a]]
groupEquals [] = []
groupEquals (x:xs) = let (group, rest) = span (== x) xs in (x : group) : groupEquals rest

hasIncreasingDigits :: Int -> Bool
hasIncreasingDigits 0 = True
hasIncreasingDigits n =
    let
        trailingDigit = n `mod` 10
        remainder = n `div` 10
    in
        trailingDigit >= (remainder `mod` 10) && hasIncreasingDigits remainder

part1 :: Int -> Int -> Int
part1 x y =
    length $ filter isValid [x .. y]
        where isValid n = isSixDigits n && hasAdjacentDigits (show n) && hasIncreasingDigits n

part2 :: Int -> Int -> Int
part2 x y =
    length $ filter isValid [x .. y]
        where
            isValid n = isSixDigits n && hasTwoAdjacentDigits (show n) && hasIncreasingDigits n
            hasTwoAdjacentDigits n = any (\xs -> length xs == 2) (groupEquals n)

day04 :: IO ()
day04 = do
    input <- readFile "data/day04-input.txt"
    let [start, end] = splitOn "-" input
    let x = read start :: Int
    let y = read end :: Int
    let res = part1 x y
    let res2 = part2 x y
    print res
    print res2