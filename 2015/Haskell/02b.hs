import Data.List (sort)


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/02.input"

solve :: [String] -> Int
solve = foldr (\str acc -> acc + (ribbonLen $ parseDims str)) 0
    where
    ribbonLen [x,y,z] = x+x+y+y + x*y*z


parseDims :: String -> [Int]
parseDims inp = sort [read x, read y, read z]
    where
    x = takeNum inp
    y = takeNum $ drop (length x + 1) inp
    z = reverse . takeNum $ reverse inp
    takeNum = takeWhile (/= 'x')
