module Day01 (day01, part1, part2, parseInput) where

import Prelude

import Data.List (sort)
import qualified Data.Map as Map

parseInput :: [String] -> [(Int, Int)]
parseInput = map parseLine
    where
        parseLine line = let [x, y] = map read (words line) in (x, y)

tupleDistance :: (Int, Int) -> Int
tupleDistance (a, b) = abs (a - b)

part1 :: [(Int, Int)] -> Int
part1 input =
    let left = sort $ map fst input
        right = sort $ map snd input
        pairs = zip left right
    in
        sum (map tupleDistance pairs)

countOccurences :: [Int] -> Map.Map Int Int
countOccurences xs = Map.fromListWith (+) [(x, 1) | x <- xs]

countSimilarityScore :: [Int] -> Map.Map Int Int -> Int
countSimilarityScore = aux 0
    where
        aux acc [] _ = acc
        aux acc (x:xs) occurences = 
            case Map.lookup x occurences of
                Nothing -> aux acc xs occurences
                Just n -> aux (acc + (x * n)) xs occurences

part2 :: [(Int, Int)] -> Int
part2 input = 
    let left = map fst input
        right = map snd input
        rightOccurences = countOccurences right
    in
        countSimilarityScore left rightOccurences


day01 :: IO ()
day01 = do
    inputLines <- lines <$> readFile "data/day01-input.txt"
    print $ part1 (parseInput inputLines)
    print $ part2 (parseInput inputLines)
