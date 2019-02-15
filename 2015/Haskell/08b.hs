main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/08.input"

solve :: [String] -> Int
solve = foldr accumulateDiff 0
    where
    accumulateDiff :: String -> Int -> Int
    accumulateDiff str chars = (+) chars $ (length $ encode str) - (length str)

encode :: String -> String
encode s = encode' ("", s)
    where
    encode' (done, "") = "\"\"" ++ done
    encode' (done, ('\\':todo)) = encode' ("\\\\" ++ done, todo)
    encode' (done, ('"':todo)) = encode' ("\\\"" ++ done, todo)
    encode' (done, todo) = encode' ('_':done, tail todo)
