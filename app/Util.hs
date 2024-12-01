module Util (getDataFileName) where

import System.FilePath

getDataFileName :: String -> FilePath
getDataFileName s = "data/" ++ s
