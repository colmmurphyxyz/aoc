module Day06 (day06) where

import Data.Set (Set)
import Data.Set qualified as S

splitWhere :: (a -> Bool) -> [a] -> [[a]]
splitWhere p x = splitWhere' p x []
  where
    splitWhere' :: (a -> Bool) -> [a] -> [[a]] -> [[a]]
    splitWhere' predicate xs acc = case break predicate xs of
      (ys, []) -> (acc ++ [ys])
      (ys, (_ : zs)) -> splitWhere' predicate (zs) (acc ++ [ys])

parseGroup :: [String] -> Set Char
parseGroup xs = S.fromList $ concat xs

part1 :: [Set Char] -> Int
part1 = sum . map S.size

parseGroup2 :: [String] -> Set Char
parseGroup2 = foldr S.intersection letters . map S.fromList
  where
    letters = S.fromAscList ['a' .. 'z']

part2 :: [Set Char] -> Int
part2 = sum . map S.size

day06 :: IO ()
day06 = do
  inp <- lines <$> readFile "data/day06-input.txt"
  let clusters = splitWhere (== "") inp
  let groups = map parseGroup clusters
  print $ part1 groups
  let groups2 = map parseGroup2 clusters
  print $ part2 groups2