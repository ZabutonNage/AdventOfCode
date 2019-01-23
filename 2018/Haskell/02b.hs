import Data.Char (ord)
import Data.List
import Data.Maybe (mapMaybe)


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/02.input"

solve :: [String] -> Maybe (String)
solve arr = findMatch charSums <&> intersectStrict
    where
    charSums = sortOn fst $ map (\word -> (getCharSum word, word)) arr
    getCharSum = sum . map ord


findMatch :: [(Int, String)] -> Maybe (String, String)
findMatch []                   = Nothing
findMatch ((charSum, word):xs) = case (getMatch word candidates) of
    Nothing -> findMatch xs
    match   -> match
    where
    candidates = map snd $ takeWhile ((<= charSum + 25) . fst) xs

getMatch :: String -> [String] -> Maybe (String, String)
getMatch _    []     = Nothing
getMatch word (x:xs)
    | diff == 1 = Just (word, x)
    | otherwise = getMatch word xs
    where
    diff = length . filter (== False) $ zipWith (==) word x

intersectStrict :: (String, String) -> String
intersectStrict (a, b) = mapMaybe sameChar $ zip a b
    where
    sameChar (x, y)
        | x == y  = Just x
        | otherwise = Nothing

(<&>) = flip fmap
