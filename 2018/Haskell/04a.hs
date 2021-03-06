import Data.List


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/04.input"

solve :: [String] -> Int
solve entries = fst sleepiest * (minuteMostAsleep [] $ snd sleepiest)
    where
    grouped = entriesByGuard [] (-1, []) $ sort entries
    sleepiest = maximumBy (\(_, as) (_, bs) -> compare (minutesAsleep 0 as) (minutesAsleep 0 bs)) grouped


type GuardEntries = (Int, [String])

entriesByGuard :: [GuardEntries] -> GuardEntries -> [String] -> [GuardEntries]
entriesByGuard acc (gid, gEntries) (e:es)
    | isInfixOf "Guard" e = entriesByGuard ((gid, reverse gEntries):acc) (idFromEntry e, []) es
    | otherwise           = entriesByGuard acc (gid, e:gEntries) es
entriesByGuard acc (gid, gEntries) [] = groupEntries . init $ sortBy entriesSorter ((gid, reverse gEntries):acc)
    where
    entriesSorter (a, _) (b, _) = let ord = compare a b in if ord == EQ then GT else ord
    groupEntries = map accumulateById . groupBy (\(a,_) (b,_) -> a == b)

accumulateById ((gid, gEntries):xs) = (gid, accEntries)
    where
    accEntries = foldr (\(_, gEntries') acc -> acc ++ gEntries') gEntries xs

idFromEntry :: String -> Int
idFromEntry entry = read idOnly
    where
    idOnly = takeWhile (/= ' ') idTail
    idTail = tail $ dropWhile (/= '#') entry

minutesAsleep :: Int -> [String] -> Int
minutesAsleep acc []       = acc
minutesAsleep acc (from:to:xs) = minutesAsleep (acc + (minutesFromEntry to - minutesFromEntry from)) xs

minuteMostAsleep :: [Int] -> [String] -> Int
minuteMostAsleep acc [] = head . maximumBy (\a b -> compare (length a) (length b)) . group . sort $ acc
minuteMostAsleep acc (from:to:xs) = minuteMostAsleep ([(minutesFromEntry from)..(minutesFromEntry to - 1)] ++ acc) xs

minutesFromEntry :: String -> Int
minutesFromEntry = read . take 2 . tail . dropWhile (/= ':')


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
