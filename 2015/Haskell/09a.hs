import Data.List (find, permutations)
import Data.Maybe (fromJust)
import Data.Set (empty, insert, toList)
import Data.Text (Text, pack, unpack, splitOn)


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/09.input"

solve :: [String] -> Int
solve inp = let (places, connections) = parseInput inp
                bidirConns = foldr (\a@(Conn x y d) conns -> a:(Conn y x d):conns) [] connections
            in shortestRoute (permutations places) bidirConns


data Conn = Conn Text Text Int

parseInput :: [String] -> ([Text], [Conn])
parseInput = mapFst toList . foldr parse (empty, [])
    where
    parse el (locs, dists) = let [first, rest] = splitOn (pack " to ") (pack el)
                                 [second, dist] = splitOn (pack " = ") rest
                             in (insert first $ insert second locs, (Conn first second (read $ unpack dist):dists))

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (l, r) = (f l, r)

shortestRoute :: [[Text]] -> [Conn] -> Int
shortestRoute routes connections = minimum $ map (distance 0) routes
    where
    distance :: Int -> [Text] -> Int
    distance total (a:b:route) = let (Conn _ _ d) = fromJust $ find (matchConnection a b) connections
                                 in distance (total + d) (b:route)
    distance total _ = total
    matchConnection a b (Conn x y _) = a == x && b == y
