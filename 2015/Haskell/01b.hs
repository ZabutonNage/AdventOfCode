main = solve <$> readFile inputFile >>= print


inputFile = "../_input/01.input"

solve :: String -> Int
solve = findPos 0 0
    where
    findPos pos (-1)  _      = pos
    findPos pos floor (x:xs) = findPos (pos + 1) (floor + (offset x)) xs
    offset '(' = 1
    offset ')' = -1
