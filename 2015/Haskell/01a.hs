main = solve <$> readFile inputFile >>= print


inputFile = "../_input/01.input"

solve :: String -> Int
solve = foldl folder 0
    where
    folder acc '(' = acc + 1
    folder acc ')' = acc - 1
