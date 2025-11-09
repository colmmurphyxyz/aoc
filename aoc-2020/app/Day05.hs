module Day05 (day05) where

import Data.List (sort)
import Prelude hiding (Left, Right)

data Vertical = Front | Back
  deriving (Eq)

instance Show Vertical where
  show Front = "F"
  show Back = "B"

verticalFromChar :: Char -> Vertical
verticalFromChar 'F' = Front
verticalFromChar 'B' = Back
verticalFromChar c = error $ "Bad argument for Vertical " ++ [c]

data Horizontal = Left | Right
  deriving (Eq)

instance Show Horizontal where
  show Left = "L"
  show Right = "R"

horizontalFromChar :: Char -> Horizontal
horizontalFromChar 'L' = Left
horizontalFromChar 'R' = Right
horizontalFromChar c = error $ "Bad argument for Horizontal " ++ [c]

data BoardingPass = BoardingPass [Vertical] [Horizontal]
  deriving (Eq)

instance Show BoardingPass where
  show (BoardingPass verts horizontals) = concat $ map show verts ++ map show horizontals

parseBoardingPass :: String -> BoardingPass
parseBoardingPass xs =
  let (verticals, horizontals) = break (`elem` "LR") xs
   in BoardingPass (map verticalFromChar verticals) (map horizontalFromChar horizontals)

verticalId :: [Vertical] -> Int
verticalId verticals = aux verticals (0, 127)
  where
    aux [] (a, _) = a
    aux (x : xs) (low, high)
      | low == high = low
      | otherwise =
          let mid = (high + low) `div` 2
           in case x of
                Front -> aux xs (low, mid)
                Back -> aux xs (mid + 1, high)

horizontalId :: [Horizontal] -> Int
horizontalId horizontals = aux horizontals (0, 7)
  where
    aux [] (_, b) = b
    aux (x : xs) (low, high)
      | low == high = low
      | otherwise =
          let mid = (high + low) `div` 2
           in case x of
                Left -> aux xs (low, mid)
                Right -> aux xs (mid + 1, high)

seatNumber :: BoardingPass -> (Int, Int)
seatNumber (BoardingPass verts horizontals) = (verticalId verts, horizontalId horizontals)

seatId :: BoardingPass -> Int
seatId bp = let (a, b) = seatNumber bp in (a * 8) + b

part1 :: [BoardingPass] -> Int
part1 = foldr max (-1) . map seatId

gap :: [Int] -> Int
gap = aux . sort
  where
    aux :: [Int] -> Int
    aux [] = error "No gaps :("
    aux [_] = error "No gaps :("
    aux (x : y : xs)
      | y == x + 1 = aux (y : xs)
      | otherwise = x + 1

part2 :: [BoardingPass] -> Int
part2 = gap . map seatId

day05 :: IO ()
day05 = do
  inp <- lines <$> readFile "data/day05-input.txt"
  let passes = map parseBoardingPass inp
  print $ part1 passes
  print $ part2 passes
