{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
module Day01 (day01) where
import Prelude hiding (product)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:xs) = zip (iterate id x) xs ++ pairs xs

product :: Num a => (a, a) -> a
product (x, y) = x * y

part1 :: [Int] -> Int
part1 nums =
    let
        candidates = filter (\(x, y) -> sum [x, y] == 2020) (pairs nums)
    in
        product $ head candidates

triples :: [a] -> [(a, a, a)]
triples [] = []
triples [_] = []
triples [_, _] = []
triples (x:xs) = 
    map (\(b, c) -> (x, b, c)) rhs ++ triples (xs)
        where rhs = pairs xs

product3 :: Num a => (a, a, a) -> a
product3 (a, b, c) = a * b * c

part2 :: [Int] -> Int
part2 nums = product3 $ head candidates
    where candidates = filter (\(x, y, z) -> sum [x, y, z] == 2020) (triples nums)

parseInput :: [String] -> [Int]
parseInput = map read

day01 :: IO ()
day01 = do
    inputLines <- lines <$> readFile "data/day01-sample.txt"
    print $ part1 (parseInput inputLines)
    print $ part2 (parseInput inputLines)
