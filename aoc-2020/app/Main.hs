module Main (main) where

import Data.Maybe (listToMaybe)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Text.Read (readMaybe)

usage :: IO ()
usage = do
  print "Usage: aoc2020 <day number>"
  print "Day must be an integer between 1 and 25"
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs
  let dayNumber = listToMaybe args >>= readMaybe :: Maybe Int
  case dayNumber of
    Nothing -> usage
    Just n -> case n of
      1 -> day01
      2 -> day02
      3 -> day03
      _ -> usage
