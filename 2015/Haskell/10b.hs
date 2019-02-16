import Data.Char (intToDigit)
import Data.List (group)


main = solve <$> readFile inputFile >>= print


inputFile = "../_input/10.input"

solve :: String -> Int
solve inp = length . foldr (\_ str -> lookAndSay str) inp . take 50 $ repeat 0


lookAndSay :: String -> String
lookAndSay str = foldr las "" $ group str
    where
    las s acc = (intToDigit $ length s):(head s):acc
