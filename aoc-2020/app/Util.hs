module Util (getInput, getInputLines, nums, to2DArray) where

import Data.Array (Array, array)
import Text.Regex.Base (getAllTextMatches)
import Text.Regex.Posix ((=~))

getInput :: FilePath -> IO String
getInput = readFile

getInputLines :: FilePath -> IO [String]
getInputLines path = lines <$> readFile path

--- List of all integers in the input string, including negative numbers
nums :: String -> [Int]
nums input = map read (getAllTextMatches (input =~ "-?[0-9]+"))

to2DArray :: [[a]] -> Array (Int, Int) a
to2DArray xss =
  array
    ((0, 0), (rows - 1, cols - 1))
    [ ((r, c), xss !! r !! c)
    | r <- [0 .. rows - 1],
      c <- [0 .. cols - 1]
    ]
  where
    rows = length xss
    cols = if null xss then 0 else length (head xss)