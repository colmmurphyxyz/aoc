{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day05 (day05, part1, part2) where

import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)

import Util (getInput)
import Data.List (partition, sortBy)

type Rule = (Int, Int)

type RuleSet = M.Map Rule ()

type Update = [Int]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim = foldr f [[]]
  where
    f c l@(x:xs)
      | c == delim = [] : l
      | otherwise = (c : x) : xs

solve :: String -> (Int, Int)
solve s = (partOne, partTwo)
    where
        (rules, updates) = let (s1, s2) = span (/= "") $ lines s in (M.fromList $ map (\a -> (parseRule a, ())) s1, map parseUpdate $ tail s2)
        (valid, invalid) = partition (isValidUpdate rules) updates
        partOne = (sum . map mid) valid
        partTwo = sum $ map (mid . sortUpdate rules) invalid

part1 :: String -> Int
part1 input =
    let (s1, _) = solve input
    in s1

part2 :: String -> Int
part2 input = 
    let (_, s2) = solve input
    in s2

parseRule :: String -> Rule
parseRule s =
    let (x, y) = span (/= '|') s
    in (read x, read $ tail y) 

parseUpdate :: String -> Update
parseUpdate s = map read $ splitOn ',' s

mid :: [a] -> a
mid a = a !! ((length a - 1) `div` 2)

isValidUpdate :: RuleSet -> Update -> Bool
isValidUpdate r u = all (\(x, y) -> isNothing $ M.lookup (y, x) r) (zip u $ tail u)

sortUpdate :: RuleSet -> Update -> Update
sortUpdate r = sortBy (\x y -> if isJust $ M.lookup (x, y) r then GT else LT)

day05 :: IO ()
day05 = do
    input <- getInput "data/day05-input.txt"
    let (s1, s2) = solve input
    print s1
    print s2