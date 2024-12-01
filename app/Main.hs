module Main where
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Day01 (day01)

main :: IO ()
main = do
    args <- getArgs
    let dayNumber = readMaybe (head args) :: Maybe Int
    case dayNumber of
        Nothing -> error "Bad Input"
        Just n -> case n of
            1 -> day01
            2 -> print "two"
            _ -> error "Invalid input arguments"