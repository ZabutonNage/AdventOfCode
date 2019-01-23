import Data.Set


main = solve <$> input >>= print


inputFile = "../_input/01.input"

input :: IO [Int]
input = fmap parse . lines <$> readFile inputFile
    where
    parse ('+':xs) = read xs
    parse xs       = read xs


solve :: [Int] -> Maybe Int
solve inp = findDouble inp [] (singleton 0) 0


findDouble :: [Int] -> [Int] -> Set Int -> Int -> Maybe Int
findDouble (x:xs) spent acc freq
    | member freqNext acc = Just freqNext
    | otherwise           = findDouble xs (x:spent) (insert freqNext acc) freqNext
    where
    freqNext = freq + x
findDouble []     spent acc freq = findDouble (reverse spent) [] acc freq
