module Day04 (day04) where

import Data.Map (Map)
import Data.Map qualified as M
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~))
import Util (count, splitOn)

splitWhere :: (a -> Bool) -> [a] -> [[a]]
splitWhere p x = splitWhere' p x []
  where
    splitWhere' :: (a -> Bool) -> [a] -> [[a]] -> [[a]]
    splitWhere' predicate xs acc = case break predicate xs of
      (ys, []) -> (acc ++ [ys])
      (ys, (_ : zs)) -> splitWhere' predicate (zs) (acc ++ [ys])

join :: String -> [String] -> String
join _ [] = ""
join _ (x : []) = x
join sep (x : xs) = x ++ sep ++ (join sep xs)

parseInput :: String -> [Map String String]
parseInput inp =
  let groups = splitWhere (== "") $ lines inp
      passportsStrings = map (splitOn ' ') $ map (join " ") groups
      tuples = map (map (break (== ':'))) passportsStrings
      tuples2 = map (map (\(a, b) -> (a, drop 1 b))) tuples
   in map M.fromList tuples2

isValid :: Map String String -> Bool
isValid passport =
  let keys = M.keys passport
   in all (`elem` keys) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 :: [Map String String] -> Int
part1 = count isValid

data Height = Centimetres Int | Inches Int

readHeight :: String -> Maybe Height
readHeight xs =
  case break (\c -> c == 'c' || c == 'i') xs of
    (x, "cm") -> Just $ Centimetres (read x :: Int)
    (x, "in") -> Just $ Inches (read x :: Int)
    (_, _) -> Nothing

isValid2 :: Map String String -> Bool
isValid2 passport =
  let byr = M.lookup "byr" passport >>= readMaybe :: Maybe Int
      iyr = M.lookup "iyr" passport >>= readMaybe :: Maybe Int
      eyr = M.lookup "eyr" passport >>= readMaybe :: Maybe Int
      hgt = M.lookup "hgt" passport >>= readHeight
      hcl = M.lookup "hcl" passport
      ecl = M.lookup "ecl" passport
      pid = M.lookup "pid" passport
   in isValid passport
        && isByrValid byr
        && isIyrValid iyr
        && isEyrValid eyr
        && isHgtValid hgt
        && isHclValid hcl
        && isEclValid ecl
        && isPidValid pid
  where
    isByrValid Nothing = False
    isByrValid (Just x) = x >= 1920 && x <= 2002
    isIyrValid (Nothing) = False
    isIyrValid (Just x) = x >= 2010 && x <= 2020
    isEyrValid Nothing = False
    isEyrValid (Just x) = x >= 2020 && x <= 2030
    isHgtValid Nothing = False
    isHgtValid (Just (Centimetres x)) = x >= 150 && x <= 193
    isHgtValid (Just (Inches x)) = x >= 59 && x <= 76
    isHclValid Nothing = False
    isHclValid (Just xs) = xs =~ "^#([0-9]|[a-f]){6}$"
    isEclValid Nothing = False
    isEclValid (Just xs) = xs `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    isPidValid Nothing = False
    isPidValid (Just xs) = xs =~ "^[0-9]{9}$"

part2 :: [Map String String] -> Int
part2 = count isValid2

day04 :: IO ()
day04 = do
  inp <- readFile "data/day04-input.txt"
  let parsedInput = parseInput inp
  print $ part1 parsedInput
  print $ part2 parsedInput