module Util (getInput, getInputLines, nums) where

import Text.Regex.Posix ((=~))
import Text.Regex.Base (getAllTextMatches)

getInput :: FilePath -> IO String
getInput = readFile 

getInputLines :: FilePath -> IO [String]
getInputLines path = lines <$> readFile path

--- List of all integers in the input string, including negative numbers
nums :: String -> [Int]
nums input = map read (getAllTextMatches (input =~ "-?[0-9]+"))