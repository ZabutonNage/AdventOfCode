import Data.List (sort)


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/02.input"

solve :: [String] -> Int
solve = foldr (\str acc -> acc + (surface $ parseDims str)) 0
    where
    surface [x,y,z] = x*y + 2*x*y + 2*x*z + 2*y*z


parseDims :: String -> [Int]
parseDims inp = sort [read x, read y, read z]
    where
    x = takeNum inp
    y = takeNum $ drop (length x + 1) inp
    z = reverse . takeNum $ reverse inp
    takeNum = takeWhile (/= 'x')
