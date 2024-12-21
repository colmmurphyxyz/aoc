module Day08 (day08, part1, part2, Coord, Frequency) where

import qualified Data.Map.Strict as M

import Util (getInputLines)

type Coord = (Int, Int)
type Frequency = Char

gridDimension :: Int
gridDimension = 50 -- change this to reflect the input size

inGrid :: Coord -> Bool
inGrid (y, x)
    | y < 0                 = False
    | y >= gridDimension    = False
    | x < 0                 = False
    | x >= gridDimension    = False
    | otherwise             = True
-- inGrid (y, x) = y >= 0 && y < gridDimension && x > 0 && x < gridDimension

coords :: [Coord]
coords = [ (y, x) | y <- [0 .. upper], x <- [0 .. upper] ]
    where upper = gridDimension - 1

charMap :: [[Frequency]] -> M.Map Frequency [Coord]
charMap grid = foldr insert M.empty coords 
    where
        insert (y, x) m = let f = grid !! y !! x in if f == '.' then m else M.insertWith (++) f [(y, x)] m

pairs :: Eq a => [a] -> [(a, a)]
pairs xs = [(x1, x2) | x1 <- xs, x2 <- xs, x1 /= x2]

antinode :: (Coord, Coord) -> Coord
antinode ((y1, x1), (y2, x2)) = (2 * y1 - y2, 2 * x1 - x2)

allAntinodes :: M.Map Frequency [Coord] -> [Coord]
allAntinodes m = filter inGrid $ nub $ concatMap (fmap antinode . pairs) $ M.elems m

antinode' :: (Coord, Coord) -> [Coord]
antinode' ((y1, x1), (y2, x2)) = filter inGrid [(y1 + n * (y2 - y1), x1 + n * (x2 - x1)) | n <- [-52..52] ]

allAntinodes' :: M.Map Frequency [Coord] -> [Coord]
allAntinodes' m = nub $ concatMap (concatMap antinode' . pairs) $ M.elems m

nub :: Eq a => [a] -> [a]
nub xs = removeDuplicates [] xs
    where
        removeDuplicates acc [] = acc
        removeDuplicates acc (x:xs) = if x `elem` acc then removeDuplicates acc xs else removeDuplicates (x : acc) xs

part1 :: [[Frequency]] -> Int
part1 = length . allAntinodes . charMap

part2 :: [[Frequency]] -> Int
part2 = length . allAntinodes' . charMap

day08 :: IO ()
day08 = do
    inputLines <- getInputLines "data/day08-input.txt"
    print $ part1 inputLines
    print $ part2 inputLines
