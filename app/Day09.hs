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

parseInput :: String -> String
parseInput = aux 0
    where
        aux :: Int -> String -> String
        aux _ "" = ""
        aux _ [_] = error ""
        aux currentBlockId (blockSize:freeSpace:xs) =
            repeatString (show currentBlockId) (digitToInt blockSize)
            ++ repeatChar '.' (digitToInt freeSpace)
            ++ aux (currentBlockId + 1) xs

toArray :: String -> Array Int Char
toArray xs = A.array (0, lastIdx) $ zip [0 .. lastIdx] xs
    where
        lastIdx = length xs - 1

firstEmptyBlockIdx :: Array Int Char -> Int
firstEmptyBlockIdx arr = go 0 (A.elems arr)
    where
        go idx (x:xs)
            | x == '.' = idx
            | otherwise = go (idx + 1) xs
        go _ [] = error "No empty block found"

lastNonEmptyBlockIdx :: Array Int Char -> Int
lastNonEmptyBlockIdx arr = go (snd (A.bounds arr))
    where
        go (-1) = error "No non-empty block found"
        go idx
            | arr ! idx /= '.' = idx
            | otherwise = go (idx - 1)

isContiguous :: Array Int Char -> Bool
isContiguous xs =
    let
        (lhs, rhs) = span (/= '.') $ A.elems xs
    in
    notElem '.' lhs && all (== '.') rhs

makeContiguous :: Array Int Char -> Array Int Char
makeContiguous arr
    | isContiguous arr = arr
    | otherwise = makeContiguous $ swapOnce arr

swapOnce :: Array Int Char -> Array Int Char
swapOnce arr =
    let
        l = firstEmptyBlockIdx arr
        r = lastNonEmptyBlockIdx arr
    in
        arr // [(l, arr ! r), (r, arr ! l)]

filesystemChecksum :: String -> Int
filesystemChecksum s =
    let nums = map digitToInt s
        size = length nums
    in
    sum $ zipWith (*) [0 .. size - 1] nums

part1 :: String -> Int
part1 inp = 
    let
        filesystem = toArray $ parseInput inp
    in
    filesystemChecksum . takeWhile (/= '.') . A.elems $ makeContiguous filesystem

day09 :: IO ()
day09 = do
    input <- (++ "0") <$> getInput "data/day09-input.txt"
    let arr = toArray $ parseInput input
    print "created array"
    let contiguousArr = makeContiguous arr
    print "is contiguous"
    print $ A.elems contiguousArr
    let checksum = filesystemChecksum . takeWhile (/= '.') $ A.elems contiguousArr
    print checksum
    -- print $ part1 input