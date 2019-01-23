import Data.List


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/02.input"

solve :: [String] -> Int
solve arr = solve' arr (0, 0)
    where
    solve' []     (doubles, triples) = doubles * triples
    solve' (x:xs) (doubles, triples) = solve' xs (doubles + (hasDoubles $ grouped x), triples + (hasTriples $ grouped x))
    grouped = group . sort


hasDoubles :: [String] -> Int
hasDoubles = hasMultiples 2

hasTriples :: [String] -> Int
hasTriples = hasMultiples 3

hasMultiples :: Int -> [String] -> Int
hasMultiples len grouped
    | 0 < length (filter ((==) len . length) grouped) = 1
    | otherwise = 0
