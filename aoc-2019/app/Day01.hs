module Day01 (day01) where

part1 :: [Int] -> Int
part1 nums = sum $ map f nums
    where f num = (num `div` 3) - 2

part2 :: [Int] -> Int
part2 = sum . map getFuelCost

getFuelCost :: Int -> Int
getFuelCost x
    | x < 9 = 0
    | otherwise = 
        let
            cost = (x `div` 3) - 2
        in
            cost + getFuelCost cost

day01 :: IO ()
day01 = do
    inputLines <- lines <$> readFile "data/day01-input.txt"
    let input = map (read :: String -> Int) inputLines
    print $ part1 input
    print $ part2 input