module Day07 (day07, part1, part2, parseLine, Equation) where

import Control.Monad (replicateM)

import Util (getInputLines)

data Equation = Equation {
    getOperands :: [Int],
    getResult :: Int
}

perms :: Int -> [[Int -> Int -> Int]]
perms n = replicateM n [(*) :: Int -> Int -> Int, (+) :: Int -> Int -> Int]

permsWithConcat :: Int -> [[Int -> Int -> Int]]
permsWithConcat n = replicateM n [(*) :: Int -> Int -> Int, (+) :: Int -> Int -> Int, concatenation]
    where
        concatenation :: Int -> Int -> Int
        concatenation lhs rhs = read (show lhs ++ show rhs) :: Int

buildExpression :: [Int] -> [Int -> Int -> Int] -> [(Int -> Int -> Int, Int)]
buildExpression operands operators =
    (identity, head operands) : f (tail operands) operators []
    where
        identity lhs rhs
            | lhs == 0 = rhs
            | rhs == 0 = lhs
            | otherwise = rhs
        f operands operators acc
            | null operands = acc
            | null operators = acc
            | otherwise = f (tail operands) (tail operators) (acc ++ [(head operators, head operands)])


evaluateExpression :: [(Int -> Int -> Int, Int)] -> Int
evaluateExpression = foldl eval 0
    where
        eval acc (op, operand) = acc `op` operand


hasValidSolution :: Equation -> Bool
hasValidSolution (Equation operands result) =
    let possibleOperators = perms (length operands - 1)
        expressions = [ buildExpression operands operators | operators <- possibleOperators]
    in
    any (\expr -> evaluateExpression expr == result) expressions

hasValidSolutionWithConcatenation :: Equation -> Bool
hasValidSolutionWithConcatenation (Equation operands result) =
    let possibleOperators = permsWithConcat (length operands - 1)
        expressions = [ buildExpression operands operators | operators <- possibleOperators]
    in
    any (\expr -> evaluateExpression expr == result) expressions


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim xs = case break (== delim) xs of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn delim after

parseLine :: String -> Equation
parseLine line = 
    let
        (resS, operandsS) = break (== ':') line
        res = read resS :: Int
        operands = map read (splitOn ' ' $ drop 2 operandsS) :: [Int]
    in Equation operands res

part1 :: [Equation] -> Int
part1 equations = sum . map getResult $ filter hasValidSolution equations

part2 :: [Equation] -> Int
part2 equations = sum . map getResult $ filter hasValidSolutionWithConcatenation equations

day07 :: IO ()
day07 = do
    inputLines <- getInputLines "data/day07-input.txt"
    let equations = map parseLine inputLines
    print $ part1 equations
    print $ part2 equations
