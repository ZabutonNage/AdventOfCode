import Data.List
import Data.Maybe (fromMaybe)


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/05.input"

solve :: [String] -> Int
solve = length . filter ((==) True . isNice)


isNice :: String -> Bool
isNice str = fromMaybe False $ do
    threeVowels str
    doubleLetter str
    noForbidden str

threeVowels :: String -> Maybe ()
threeVowels str
    | (length $ str `intersect` vowels) >= 3 = Just ()
    | otherwise                              = Nothing

doubleLetter :: String -> Maybe ()
doubleLetter str
    | noDouble = Nothing
    | otherwise = Just ()
    where
    noDouble = null . dropWhile ((>) 2 . length) $ group str

noForbidden :: String -> Maybe Bool
noForbidden str
    | any (`isInfixOf` str) forbidden = Nothing
    | otherwise = Just True


vowels = ['a', 'e', 'i', 'o', 'u']
forbidden = ["ab", "cd", "pq", "xy"]
