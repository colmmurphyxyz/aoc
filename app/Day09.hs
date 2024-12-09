module Day09 where

import Data.Char (digitToInt)
import GHC.Arr (Array, (!), (//))
import qualified GHC.Arr as A

import Util (getInput)

repeatChar :: Char -> Int -> String
repeatChar c n
    | n <= 0 = ""
    | otherwise = c : repeatChar c (n - 1)

repeatString :: String -> Int -> String
repeatString s n
    | n <= 0 = ""
    | otherwise = s ++ repeatString s (n - 1)

repeatN :: Int -> a -> [a]
repeatN n x
    | n <= 0 = []
    | otherwise = x : repeatN (n - 1) x

parseInput :: String -> [Int]
parseInput s = 
    let nums = map digitToInt s
    in aux 0 nums
    where
        aux :: Int -> [Int] -> [Int]
        aux _ [] = []
        aux _ [_] = error "Input length must be even"
        aux currentBlockId (blockSize:freeSpace:xs) =
            repeatN blockSize currentBlockId
            ++ repeatN freeSpace (-1)
            ++ aux (currentBlockId + 1) xs

toArray :: [a] -> Array Int a
toArray xs = A.array (0, lastIdx) $ zip [0 .. lastIdx] xs
    where
        lastIdx = length xs - 1

firstEmptyBlockIdx :: Array Int Int -> Int
firstEmptyBlockIdx arr = go 0 (A.elems arr)
    where
        go idx (x:xs)
            | x == -1 = idx
            | otherwise = go (idx + 1) xs
        go _ [] = error "No empty block found"

lastNonEmptyBlockIdx :: Array Int Int -> Int
lastNonEmptyBlockIdx arr = go (snd (A.bounds arr))
    where
        go (-1) = error "No non-empty block found"
        go idx
            | arr ! idx /= -1 = idx
            | otherwise = go (idx - 1)

isContiguous :: Array Int Int -> Bool
isContiguous xs =
    let
        (lhs, rhs) = span (/= -1) $ A.elems xs
    in
    notElem (-1) lhs && all (== -1) rhs

makeContiguous :: Array Int Int -> Array Int Int
makeContiguous arr
    | isContiguous arr = arr
    | otherwise = makeContiguous $ swapOnce arr

swapOnce :: Array Int Int -> Array Int Int
swapOnce arr =
    let
        l = firstEmptyBlockIdx arr
        r = lastNonEmptyBlockIdx arr
    in
        arr // [(l, arr ! r), (r, arr ! l)]

filesystemChecksum :: [Int] -> Int
filesystemChecksum nums =
    let
        size = length nums
    in
    sum $ zipWith (*) [0 .. size - 1] nums

part1 :: String -> Int
part1 inp = 
    let
        filesystem = toArray $ parseInput inp
    in
    filesystemChecksum . takeWhile (/= -1) . A.elems $ makeContiguous filesystem

-- FIXME file IDs will not always be one digit long. Fix this!
day09 :: IO ()
day09 = do
    input <- (++ "0") <$> getInput "data/day09-input.txt"
    print $ part1 input