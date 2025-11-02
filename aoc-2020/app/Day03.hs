module Day03 (day03) where

import Data.Array (Array, bounds, (!))
import Debug.Trace (trace)
import Util (to2DArray)

data Object = Tree | Open deriving (Eq)

instance Show Object where
  show Tree = "#"
  show Open = "."

type Grid = Array (Int, Int) Object

type Slope = (Int, Int)

gridHeight :: Grid -> Int
gridHeight grid = upper - lower + 1
  where
    b = bounds grid
    upper = fst . snd $ b
    lower = fst . fst $ b

gridWidth :: Grid -> Int
gridWidth grid = upper - lower + 1
  where
    b = bounds grid
    upper = snd . snd $ b
    lower = snd . fst $ b

getPoint :: Grid -> (Int, Int) -> Object
getPoint grid (y, x) =
  let width = gridWidth grid
   in grid ! (y, x `mod` width)

mkGridLine :: String -> [Object]
mkGridLine line = map mkObject line
  where
    mkObject '#' = Tree
    mkObject _ = Open

isObstacle :: Grid -> (Int, Int) -> Bool
isObstacle grid point = getPoint grid point == Tree

points :: Int -> Slope -> [(Int, Int)]
points maxHeight (dy, dx) = takeWhile heightIsValid $ iterate (\(y, x) -> (y + dy, x + dx)) (dy, dx)
  where
    heightIsValid (y, _) = y < maxHeight

part1 :: Grid -> Int
part1 grid =
  let height = gridHeight grid
      positions = points height (1, 3)
   in length . filter (isObstacle grid) $ positions

part2 :: Grid -> Int
part2 grid = foldr (*) 1 [solve (1, 1), solve (1, 3), solve (1, 5), solve (1, 7), solve (2, 1)]
  where
    solve slope = length . filter (isObstacle grid) $ points (gridHeight grid) slope

day03 :: IO ()
day03 = do
  inputLines <- lines <$> readFile "data/day03-input.txt"
  let objects = map mkGridLine inputLines
  let grid :: Grid = to2DArray objects
  print $ part1 grid
  print $ part2 grid
