module Main where
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)

main :: IO ()
main = do
    args <- getArgs
    let dayNumber = readMaybe (head args) :: Maybe Int
    case dayNumber of
        Nothing -> error "Bad Input"
        Just n -> case n of
            1 -> day01
            2 -> day02
            3 -> day03
            4 -> day04
            _ -> error "Invalid input arguments"