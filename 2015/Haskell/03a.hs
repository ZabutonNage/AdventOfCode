import Data.Set (Set)
import qualified Data.Set as Set


main = solve <$> readFile inputFile >>= print


inputFile = "../_input/03.input"

solve :: [Char] -> Int
solve = travel (Set.singleton startingPoint) startingPoint
    where
    startingPoint = (0, 0)


travel :: Set (Int, Int) -> (Int, Int) -> [Char] -> Int
travel visited _    []     = Set.size visited
travel visited prev (x:xs) = travel (Set.insert (next prev x) visited) (next prev x) xs

next (x, y) step
    | step == '^' = (x, y-1)
    | step == '>' = (x+1, y)
    | step == 'v' = (x, y+1)
    | step == '<' = (x-1, y)
