module Util (getInput, getInputLines) where

getInput :: FilePath -> IO String
getInput = readFile 

getInputLines :: FilePath -> IO [String]
getInputLines path = lines <$> readFile path