{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Day04 (day04) where

import Data.List (tails, isPrefixOf, transpose, sort)
import qualified Data.Map as Map

import Util (getInputLines)

type Grid = Map.Map (Int, Int) Char

getCoordinates :: Int -> Int -> [(Int, Int)]
getCoordinates height width =
    [ (h, w) | h <- [0..height-1], w <- [0..width-1] ]

zipWithCoordinates :: [(Int, Int)] -> [[a]] -> [((Int, Int), a)]
zipWithCoordinates coords xs = [ ((i, j), getValue i j) | (i, j) <- coords]
    where
        getValue i j = (xs !! i) !! j

buildMap :: [[Char]] -> Grid
buildMap xxs =
    let height = length xxs
        width = length (head xxs)
        coordinates = getCoordinates height width
        pairs = zipWithCoordinates coordinates xxs
    in Map.fromList pairs

searchGrid :: Grid -> Int
searchGrid grid = length $ filter checkCellX (Map.keys grid)
    where
        checkCellX (i, j) = case Map.lookup (i, j) grid of
            Just 'X' -> True
            _ -> False


part1 :: [String] -> Int
part1 _ = 0

day04 :: IO ()
day04 = do
    input <- getInputLines "data/day04-sample.txt"
    let grid = buildMap input
    _ <- error "day 4 not implemented"
    print $ part1 input