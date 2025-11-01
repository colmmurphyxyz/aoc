module Day02 (day02) where

import Control.Exception (displayException)
import Text.ParserCombinators.Parsec

data Password = Password Int Int Char String deriving (Eq, Show)

password :: GenParser Char st [Password]
password = do
    result <- many line
    eof
    skipMany $ oneOf " \n"
    return result

line :: GenParser Char st Password
line = do
    minOccurences <- numeric
    skipMany1 $ char '-'
    maxOccurences <- numeric
    spaces
    requiredLetter <- lower
    skipMany $ char ':'
    spaces
    passwordText <- many1 $ lower
    _ <- newline
    return (Password minOccurences maxOccurences requiredLetter passwordText)

numeric :: GenParser Char st Int
numeric = read <$> many1 digit

parsePasswords :: String -> Either ParseError [Password]
parsePasswords input = parse password "(unknown)" input

occurences :: (Eq a) => a -> [a] -> Int
occurences x xs = length $ filter (== x) xs

isValid :: Password -> Bool
isValid (Password minOccs maxOccs requiredLetter passwordText) = (occs >= minOccs) && (occs <= maxOccs)
    where
        occs = occurences requiredLetter passwordText

part1 :: [Password] -> Int
part1 = length . filter isValid

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

isValid2 :: Password -> Bool
isValid2 (Password pos1 pos2 character passwordText) = (passwordText !! (pos1 - 1) == character) `xor` (passwordText !! (pos2 - 1) == character)

part2 :: [Password] -> Int
part2 = length . filter isValid2

day02 :: IO ()
day02 = do
    input <- readFile "data/day02-input.txt"
    let passwords = parsePasswords input
    case passwords of
        Left parseError -> error $ displayException parseError
        Right p -> do
            print $ part1 p
            print $ part2 p