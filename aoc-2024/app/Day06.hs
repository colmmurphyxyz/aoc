module Day06 where

import Prelude hiding (Left, Right)
import qualified Data.Set as Set

import Util (getInputLines)

type Coord = (Int, Int)

data Object = Obstacle
    | Clear
    | Guard
    deriving (Eq, Ord, Show)

type Grid = Set.Set (Coord, Object)

data Direction = Up
    | Right 
    | Down
    | Left
    deriving (Eq, Show)

turn :: Direction -> Direction
turn Up = Right
turn Right = Down
turn Down = Left
turn Left = Up

data Chart = Chart {
    grid :: Grid,
    direction :: Direction
}

parseInput :: [String] -> Grid
parseInput inputLines =
    let height = length inputLines
        width = length $ head inputLines
        rows = [0 .. height - 1]
        x = zip rows inputLines
        gridLines = map (\a -> parseInputLine (fst a) width (snd a)) x
    in foldr Set.union Set.empty gridLines

makeChart :: Grid -> Chart
makeChart g = Chart g Up


parseInputLine :: Int -> Int -> String -> Grid
-- parseInputLine rowIdx rowLength line = Set.fromList $ [ ((rowIdx, colIdx), toObject x) | colIdx <- [0..rowLength], x <- line]
parseInputLine rowIdx rowLength line =
    let coords = [ (rowIdx, colIdx) | colIdx <- [0 .. rowLength - 1] ]
        objects = map toObject line
    in
    Set.fromList (zip coords objects)


toObject :: Char -> Object
toObject '#' = Obstacle
toObject '^' = Guard
toObject _ = Clear

day06 :: IO ()
day06 = do
    input <- getInputLines "data/day06-sample.txt"
    let size = (length input, length $ head input)
    let grid = parseInput input
    print size
    print $ length grid
    print grid
    print "done"