module Util (getInput) where

import System.FilePath

getInput :: FilePath -> IO [String]
getInput path = lines <$> readFile path