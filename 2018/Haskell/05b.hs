import Data.Char (toLower)
import Data.List (minimum)


main = solve <$> readFile inputFile >>= print


inputFile = "../_input/05.input"

solve :: String -> Int
solve polymer = minimum $ ['a'..'z'] <&> \c -> length . react [] $ filter ((/=) c . toLower) polymer


react :: [Char] -> String -> String
react stack@[]     (y:ys) = react (y:stack) ys
react stack@(x:xs) (y:ys)
    | x /= y && toLower x == (toLower y) = react xs ys
    | otherwise = react (y:stack) ys
react stack [] = reverse stack


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
