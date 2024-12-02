module Day02(day02, part1, part2, parseInput) where

import Prelude

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

data Monotonicity = Increasing
    | Decreasing
    | None

getMonotonicity :: [Int] -> Monotonicity
getMonotonicity xs
    | isIncreasing xs = Increasing
    | isDecreasing xs = Decreasing
    | otherwise = None

isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x:y:xs) = x < y && isIncreasing (y:xs)

isDecreasing :: [Int] -> Bool
isDecreasing [] = True
isDecreasing [_] = True
isDecreasing (x:y:xs) = x > y && isDecreasing (y:xs)

part1 :: [[Int]] -> Int
part1 = count isSafe

isSafe :: [Int] -> Bool
isSafe xs = case getMonotonicity xs of
    Increasing -> hasSafeLevelChanges xs
    Decreasing -> hasSafeLevelChanges xs
    None -> False

hasSafeLevelChanges :: [Int] -> Bool
hasSafeLevelChanges xs = all (\x -> x >= 1 && x <= 3) (absDiffs xs)
    where
        absDiffs [] = []
        absDiffs [_] = []
        absDiffs (x:y:ys) = abs (x - y) : absDiffs (y:ys)


part2 :: [[Int]] -> Int
part2 = count isSafe'
    where
        isSafe' xs = any (isSafe . removeAt xs) [0 .. length xs - 1]
        removeAt xs i = take i xs ++ drop (i + 1) xs

parseInput :: [String] -> [[Int]]
parseInput = map parseLine
    where
        parseLine line =
            map read (words line)

day02 :: IO ()
day02 = do
    inputLines <- lines <$> readFile "data/day02-input.txt"
    let input = parseInput inputLines
    print $ part1 input
    print $ part2 input
