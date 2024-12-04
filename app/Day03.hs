{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day03 (day03, part1, part2) where

import Prelude
import Text.Regex.Posix

import Util (getInput)

getMatches :: String -> String -> [String]
getMatches pattern input = getAllTextMatches (input =~ pattern :: AllTextMatches [] String)

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [_] = []
getPairs (x:y:xs) = (x, y) : getPairs xs

part1 :: String -> Int
part1 input =
    let pattern = "(mul\\([0-9]+,[0-9]+\\))"
        matches = getMatches pattern input
        stringOperands = concatMap (getMatches "([0-9]+)") matches
        operands = getPairs (map read stringOperands :: [Int])
    in sum $ map (uncurry (*)) operands

getMultiplications :: [String] -> [String]
getMultiplications input = parse input True 
    where
        parse [] _ = []
        parse (x:xs) doMultiplications = case take 3 x of
            "mul" -> if doMultiplications then x : parse xs True else parse xs False
            "do(" -> parse xs True
            "don" -> parse xs False

part2 :: String -> Int
part2 input = do
    let pattern = "(mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\))"
    let allOperations = getMatches pattern input
    let mulExpressions = getMultiplications allOperations
    let mulOps = concatMap (getMatches "([0-9]+)") mulExpressions
    let operands = getPairs (map read mulOps :: [Int])
    sum $ map (uncurry (*)) operands



day03 :: IO ()
day03 = do
    input <- getInput "data/day03-input.txt"
    print $ part1 input
    print $ part2 input
