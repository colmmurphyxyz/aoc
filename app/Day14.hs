module Day14 where

import Util (getInputLines, nums)

data Guard = Guard {
    getPosition :: Position,
    getVelocity :: Velocity
}

type Position = (Int, Int)
type Velocity = (Int, Int)

width :: Int
width = 101
-- width = 11

height :: Int
height = 103
-- height = 7

parseLine :: String -> Guard
parseLine line =
    let [px, py, vx, vy] = nums line
    in Guard (px, py) (vx, vy)

simulateOneSecond :: Guard -> Guard
simulateOneSecond guard =
    let (px, py) = getPosition guard
        (vx, vy) = getVelocity guard
        newPos = (px + vx, py + vy)
    in
    Guard (teleported newPos) (vx, vy)

teleported :: Position -> Position
teleported (x, y) = (tx x, ty y)
    where
        tx i
            | i < 0 = width + i
            | i >= width = i `mod` width
            | otherwise = i
        ty i
            | i < 0 = height + i
            | i >= height = i `mod` height
            | otherwise = i

simulations :: [Guard] -> [[Guard]]
simulations = iterate (map simulateOneSecond)

safetyFactor :: [Guard] -> Int
safetyFactor guards = 
    let
        topLeft = length $ filter inTopLeft guards
        topRight = length $ filter inTopRight guards
        bottomLeft = length $ filter inBottomLeft guards
        bottomRight = length $ filter inBottomRight guards
    in
    topLeft * topRight * bottomLeft * bottomRight
        where
            inTopLeft :: Guard -> Bool
            inTopLeft g = let (x, y) = getPosition g in x < width `div` 2 && y < height `div` 2
            inTopRight :: Guard -> Bool
            inTopRight g = let (x,  y) = getPosition g in x > width `div` 2 && y < height `div` 2
            inBottomLeft :: Guard -> Bool
            inBottomLeft g = let (x, y) = getPosition g in x < width `div` 2 && y > height `div` 2
            inBottomRight :: Guard -> Bool
            inBottomRight g = let (x, y) = getPosition g in x > width `div` 2 && y > height `div` 2

part1 :: [Guard] -> Int
part1 guards = safetyFactor $ simulations guards !! 100

part2 :: [Guard] -> String
part2 guards = 
    let diagrams = map showGuards $ take 200 $ simulations guards
        labelledDiagrams = zip diagrams ([0 ..] :: [Int])
    in unlines $ map (\(d, i) -> "Iteration " ++ show i ++ "\n" ++ d) labelledDiagrams


showGuards:: [Guard] -> String
showGuards guards = 
    unlines $ map showLine [0 .. height - 1]
        where
            showLine y = map (getSymbol y) [0 .. width - 1]
            getSymbol y x = 
                if any (\g -> getPosition g == (x, y)) guards then '#' else ' '

joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs


day14 :: IO ()
day14 = do
    inp <- getInputLines "data/day14-input.txt"
    let guards = map parseLine inp
    print $ part1 guards
    putStrLn $ part2 guards
