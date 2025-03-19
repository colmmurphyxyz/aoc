{-# LANGUAGE TupleSections #-}
module Day03 (day03) where

import Prelude hiding (Left, Right)
import Text.Printf (printf)
import Data.Set (Set)
import qualified Data.Set as S

data Direction = Up
    | Down
    | Left
    | Right
    deriving (Eq)

instance Read Direction where
    readsPrec _ "U" = [(Up, "")]
    readsPrec _ "D" = [(Down, "")]
    readsPrec _ "L" = [(Left, "")]
    readsPrec _ "R" = [(Right, "")]
    readsPrec _ xs = error $ printf "%s is Not a valid direction" xs

instance Show Direction where
    show Up = "U"
    show Down = "D"
    show Left = "L"
    show Right = "R"

data Action = Action {
    getDirection :: Direction,
    getMagnitude :: Int
} deriving (Eq)

instance Show Action where
    show (Action direction magnitude) = show direction ++ show magnitude

instance Read Action where
    readsPrec _ input = [(Action (read direction) (read magnitude), "")]
        where (direction, magnitude) = span (`elem` "UDLR") input

parseInput :: String -> [Action]
parseInput xs = map read (splitOn ',' xs)
    where
        splitOn _ [] = []
        splitOn delimiter str =
            let (first, remainder) = span (/= delimiter) str
            in first : case remainder of
                [] -> []
                x -> splitOn delimiter (tail x)

type Point = (Int, Int)

nextPoint :: Point -> Action -> [Point]
nextPoint (x, y) (Action Up mag) = [ (x, y + i) | i <- [1 .. mag] ]
nextPoint (x, y) (Action Down mag) = [ (x, y - i) | i <- [1 .. mag] ]
nextPoint (x, y) (Action Left mag) = [ (x - i, y) | i <- [1 .. mag] ]
nextPoint (x, y) (Action Right mag) = [ (x + i, y) | i <- [1 .. mag] ]

unroll :: Point -> Action -> (Point, [Point])
unroll point action =
    let
        points = nextPoint point action
    in
        (last points, points)

toPoints :: Point -> [Action] -> [Point]
toPoints _ [] = []
toPoints startPoint (x:xs) =
    let
        (next, points) = unroll startPoint x
    in
        points ++ toPoints next xs

manhattanDist :: Point -> Int
manhattanDist (a, b) = abs a + abs b

part1 :: [[Action]] -> Int
part1 actions =
    let
        points = map (toPoints (0, 0)) actions
        sets = map S.fromList points
        crosses = foldr1 S.intersection sets
    in
        -- S.elems crosses
        minimum $ map manhattanDist (S.elems crosses)

day03 :: IO ()
day03 = do
    inputLines <- lines <$> readFile "data/day03-input.txt"
    let actions = map parseInput inputLines
    let ans = part1 actions
    print ans