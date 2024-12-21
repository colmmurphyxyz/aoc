numDigits :: Integral a => a -> Int
numDigits n
    | n < 10 = 1
    | otherwise = 1 + numDigits (n `div` 10)

splitFast :: Int -> [Int]
splitFast n = 
    let
        half = 10 ^ (numDigits n `div` 2)
        lhs = n `div` half
        rhs = n `mod` half
    in
        [lhs, rhs]

split :: Int -> [Int]
split n = 
    let
        s = show n
        half = (numDigits n) `div` 2
        lhs = take half s
        rhs = drop half s
    in
        map read [lhs, rhs]