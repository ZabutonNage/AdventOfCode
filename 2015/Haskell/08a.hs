import Data.List (isPrefixOf)


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/08.input"

solve :: [String] -> Int
solve = foldr accumulateDiff 0
    where
    accumulateDiff :: String -> Int -> Int
    accumulateDiff str chars = (+) chars $ (length str) - (length . decode . init $ tail str)

decode :: String -> String
decode s = decode' ("", s)
    where
    decode' (done, "") = done
    decode' (done, todo)
        | "\\\\" `isPrefixOf` todo = decode' ('_':done, drop 2 todo)
        | "\\\"" `isPrefixOf` todo = decode' ('_':done, drop 2 todo)
        | "\\x" `isPrefixOf` todo = decode' ('_':done, drop 4 todo)
        | otherwise = decode' ('_':done, tail todo)
