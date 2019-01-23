import Data.Set (Set)
import qualified Data.Set as Set


main = solve <$> readFile inputFile >>= print


inputFile = "../_input/03.input"

solve :: [Char] -> Int
solve = Set.size . travelBoth . splitPath ([], [])
    where
    travelBoth (santa, robo) = travel (Set.singleton start) start santa `travel` start $ robo
    start = (0, 0)


travel :: Set (Int, Int) -> (Int, Int) -> [Char] -> Set (Int, Int)
travel visited _    []     = visited
travel visited prev (x:xs) = travel (Set.insert (next prev x) visited) (next prev x) xs

next (x, y) step
    | step == '^' = (x, y-1)
    | step == '>' = (x+1, y)
    | step == 'v' = (x, y+1)
    | step == '<' = (x-1, y)

splitPath :: ([Char], [Char]) -> [Char] -> ([Char], [Char])
splitPath (santa, robo) (s:r:xs) = splitPath (s:santa, r:robo) xs
splitPath (santa, robo) (s:[])   = splitPath (s:santa, robo) []
splitPath (santa, robo) []       = (reverse santa, reverse robo)
