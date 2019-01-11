main = print . solve $ inpToLines input


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


input = "L5, R1, L5, L1, R5, R1, R1, L4, L1, L3, R2, R4, L4, L1, L1, R2, R4, R3, L1, R4, L4, L5, L4, R4, L5, R1, R5, L2, R1, R3, L2, L4, L4, R1, L192, R5, R1, R4, L5, L4, R5, L1, L1, R48, R5, R5, L2, R4, R4, R1, R3, L1, L4, L5, R1, L4, L2, L5, R5, L2, R74, R4, L1, R188, R5, L4, L2, R5, R2, L4, R4, R3, R3, R2, R1, L3, L2, L5, L5, L2, L1, R1, R5, R4, L3, R5, L1, L3, R4, L1, L3, L2, R1, R3, R2, R5, L3, L1, L1, R5, L4, L5, R5, R2, L5, R2, L1, L5, L3, L5, L5, L1, R1, L4, L3, L1, R2, R5, L1, L3, R4, R5, L4, L1, R5, L1, R5, R5, R5, R2, R1, R2, L5, L5, L5, R4, L5, L4, L4, R5, L2, R1, R5, L1, L5, R4, L3, R4, L2, R3, R3, R3, L2, L2, L2, L1, L4, R3, L4, L2, R2, R5, L1, R2"
