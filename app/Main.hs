module Main where
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode( ExitFailure ))
import Text.Read (readMaybe)

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day11 (day11)
import Day14 (day14)

usage :: IO ()
usage = do
    print "Usage: aoc2024 <DAY_NUMBER>"
    print "Day must be an integer between 1 and 25."
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    args <- getArgs
    let dayNumber = readMaybe (head args) :: Maybe Int
    case dayNumber of
        Nothing -> usage
        Just n -> case n of
            1 -> day01
            2 -> day02
            3 -> day03
            4 -> day04
            5 -> day05
            6 -> day06
            7 -> day07
            8 -> day08
            9 -> day09
            10 -> error "not implemented"
            11 -> day11
            12 -> error "not implemented"
            13 -> error "not implemented"
            14 -> day14
            15 -> error "not implemented"
            16 -> error "not implemented"
            17 -> error "not implemented"
            18 -> error "not implemented"
            19 -> error "not implemented"
            20 -> error "not implemented"
            21 -> error "not implemented"
            22 -> error "not implemented"
            23 -> error "not implemented"
            24 -> error "not implemented"
            25 -> error "not implemented"
            _ -> usage