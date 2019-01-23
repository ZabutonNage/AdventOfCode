main = solve . inpToLines <$> readFile inputFile >>= print


inputFile = "../_input/01.input"

solve :: [String] -> Int
solve = posToDist . snd . foldl relocate (U, (0, 0))
    where
    relocate :: (Dir, Pos) -> String -> (Dir, Pos)
    relocate (dir, pos) ('R':dist) = (right dir, move (right dir) (read dist) pos)
    relocate (dir, pos) ('L':dist) = (left dir, move (left dir) (read dist) pos)


data Dir = U | R | D | L
type Pos = (Int, Int)

posToDist :: Pos -> Int
posToDist (x, y) = abs x + abs y

move :: Dir -> Int -> Pos -> Pos
move U dist (x, y) = (x, y + dist)
move R dist (x, y) = (x + dist, y)
move D dist (x, y) = (x, y - dist)
move L dist (x, y) = (x - dist, y)

right :: Dir -> Dir
right U = R
right R = D
right D = L
right L = U

left :: Dir -> Dir
left U = L
left L = D
left D = R
left R = U

inpToLines :: String -> [String]
inpToLines = lines . foldr step ""
    where
    step ' ' acc = acc
    step ',' acc = '\n':acc
    step c   acc = c:acc
