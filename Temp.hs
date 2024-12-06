module Temp where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim = foldr f [[]]
  where
    f c l@(x:xs)
      | c == delim = [] : l
      | otherwise = (c : x) : xs
