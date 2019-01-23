import Data.List
import Data.Maybe (fromMaybe)


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/05.input"

solve :: [String] -> Int
solve = length . filter ((==) True . isNice)


isNice :: String -> Bool
isNice str = fromMaybe False $ do
    twoPairs str
    xyxPattern str

twoPairs :: String -> Maybe ()
twoPairs (x:y:xs)
    | (x:y:[]) `isInfixOf` xs = Just ()
    | otherwise = twoPairs $ y:xs
twoPairs _ = Nothing

xyxPattern :: String -> Maybe Bool
xyxPattern (x:y:z:xs)
    | x == z    = Just True
    | otherwise = xyxPattern (y:z:xs)
xyxPattern _ = Nothing
