module Util (getInput, getInputLines) where

import System.FilePath

getInput :: FilePath -> IO String
getInput = readFile 

getInputLines :: FilePath -> IO [String]
getInputLines path = lines <$> readFile path