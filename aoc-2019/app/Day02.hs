module Day02 (day02) where

import Data.Array
import Text.Printf (printf)
import Data.List (iterate')

import Util (nums)

type Address = Int

type Memory = Array Address Int

data Machine = Machine
  { getMemory :: !Memory
  , getIp :: !Int
  , isHalt :: !Bool
  } deriving (Show)

makeMachine :: [Int] -> Int -> Int -> Machine
makeMachine xs noun verb =
    let
        n = length xs
        memory = array (0, n - 1) (zip [0 .. ] xs) // [(1, noun), (2, verb)]
    in
    Machine memory 0 False

step :: Machine -> Machine
step machine@(Machine _ _ True) = machine
step machine@(Machine memory ip _) =
    let opcode = memory ! getIp machine
        in case opcode of
            1 ->
                let a1 = memory ! (memory ! (ip + 1))
                    a2 = memory ! (memory ! (ip + 2))
                    dst = memory ! (ip + 3)
                in machine { getMemory = memory // [(dst, a1 + a2)], getIp = ip + 4 }
            2 ->
                let a1 = memory ! (memory ! (ip + 1))
                    a2 = memory ! (memory ! (ip + 2))
                    dst = memory ! (ip + 3)
                in machine { getMemory = memory // [(dst, a1 * a2)], getIp = ip + 4 }
            99 -> machine { isHalt = True}
            _ -> error $ printf "Unknown opcode `%d` at position `%d`" opcode ip

execute :: Machine -> Machine
execute = head . dropWhile (not . isHalt) . iterate' step

output :: Machine -> Int
output = head . elems . getMemory

part1 :: [Int] -> Int
part1 nums =
    let
        machine = makeMachine nums
        final = execute $ machine 12 2
    in
        (head . elems . getMemory) final

part2 :: [Int] -> Int
part2 xs =
  head $
  [ 100 * noun + verb
  | noun <- [0 .. 99]
  , verb <- [0 .. 99]
  , output (execute (makeMachine xs noun verb)) == 19690720
  ]


day02 :: IO ()
day02 = do
    input <- nums <$> readFile "data/day02-input.txt"
    print $ part1 input
    print $ part2 input
