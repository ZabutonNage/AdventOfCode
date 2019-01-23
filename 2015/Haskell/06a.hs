import Data.Char (isDigit)
import Data.List (foldl', isPrefixOf)
import qualified Data.Map.Strict as Map


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/06.input"

solve :: [String] -> Int
solve inp = countLights . foldl' switchLights makeGrid $ parseInstr <$> inp


countLights :: Grid -> Int
countLights = foldr (\xs acc -> acc + (sum xs)) 0

makeGrid :: Grid
makeGrid = Map.fromAscList $ [0..999] <&> \k -> (k, take 1000 $ repeat 0)

parseInstr :: String -> Instr
parseInstr instr = Instr action fstCoord sndCoord
    where
    action
        | "turn on" `isPrefixOf` instr   = TurnOn
        | "turn off" ` isPrefixOf` instr = TurnOff
        | otherwise                      = Toggle
    fstCoord = coordFromString . takeWhile (/= ' ') $ dropWhile (not . isDigit) instr
    sndCoord = coordFromString . reverse . takeWhile (/= ' ') $ reverse instr
    coordFromString s = case span (/= ',') s of
        (x, y) -> (read x, read $ tail y)

switchLights :: Grid -> Instr -> Grid
switchLights grid (Instr action (x1,y1) (x2,y2)) = foldl' (\g x -> Map.adjust switch x g) grid [x1..x2]
    where
    switch ys = (before ys) ++ (switch' action $ middle ys) ++ (after ys)
    switch' TurnOn = map (const 1)
    switch' TurnOff = map (const 0)
    switch' Toggle = map (1 -)
    before = take y1
    middle = take (y2 - y1 + 1) . drop y1
    after = drop (y2 + 1)


type Grid = Map.Map Int [Int]
data Instr = Instr Action Point Point
    deriving (Show)
data Action = TurnOn | TurnOff | Toggle
    deriving (Show)
type Point = (Int, Int)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
