main = solve <$> input >>= print


inputFile = "../_input/01.input"

input :: IO [Int]
input = fmap parse . lines <$> readFile inputFile
    where
    parse ('+':xs) = read xs
    parse xs       = read xs


solve :: [Int] -> Int
solve = foldr (+) 0
