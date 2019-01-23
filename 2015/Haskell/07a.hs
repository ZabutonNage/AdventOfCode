import Data.Bits
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (isInfixOf, isPrefixOf, sortBy)
import Data.Word (Word16)
import Data.Map.Strict (Map, empty, insert, (!))


main = solve . lines <$> readFile inputFile >>= print


inputFile = "../_input/07.input"

solve :: [String] -> Maybe Word16
solve inp = fmap (resolve resolved . parseSource) $ lookup "a" wires
    where
    resolved = resolveSources empty $ tail wires
    wires = sortBy sortWires $ toKV <$> inp
    sortWires ([], _) ([], _) = EQ
    sortWires (a:_, _) ([], _) = GT
    sortWires ([], _) (b:_, _) = LT
    sortWires (a:as, _) (b:bs, _)
        | a == b = sortWires (as, []) (bs, [])
        | length as /= length bs = compare (length as) (length bs)
        | otherwise = compare a b


type ResolvedMap = Map String Word16

resolveSources :: ResolvedMap -> [(String, String)] -> ResolvedMap
resolveSources resolved [] = resolved
resolveSources resolved ((key, val):wires) = resolveSources (nextMap $ parseSource val) wires
    where
    nextMap (Const v) = insert key v                      resolved
    nextMap src       = insert key (resolve resolved src) resolved

resolve :: ResolvedMap -> Source -> Word16
resolve _ (Const val) = val
resolve m (Wire w) = m ! w
resolve m (Gate (Not s)) = complement $ resolve m s
resolve m (Gate (And s1 s2)) = (resolve m s1) .&. (resolve m s2)
resolve m (Gate (Or s1 s2)) = (resolve m s1) .|. (resolve m s2)
resolve m (Gate (ShiftL s n)) = shiftL (resolve m s) n
resolve m (Gate (ShiftR s n)) = shiftR (resolve m s) n

parseSource :: String -> Source
parseSource str
    | all isDigit str = Const $ read str
    | all isAlpha str = Wire str
    | "NOT" `isPrefixOf` str = Gate . Not . parseSource $ drop 4 str
    | "AND" `isInfixOf` str = Gate $ And (parseSource $ leftSrc str) (parseSource $ rightSrc str)
    | "OR" `isInfixOf` str = Gate $ Or (parseSource $ leftSrc str) (parseSource $ rightSrc str)
    | "LSHIFT" `isInfixOf` str = Gate $ ShiftL (parseSource $ leftSrc str) (read $ rightSrc str)
    | "RSHIFT" `isInfixOf` str = Gate $ ShiftR (parseSource $ leftSrc str) (read $ rightSrc str)
    where
    leftSrc = takeWhile isAlphaNum
    rightSrc = reverse . takeWhile isAlphaNum . reverse

toKV :: String -> (String, String)
toKV str = (key, val)
    where
    key = drop 2 $ dropWhile (/= '>') str
    val = init $ takeWhile (/= '-') str


data Source
    = Const Word16
    | Wire String
    | Gate Gate

data Gate
    = Not Source
    | And Source Source
    | Or Source Source
    | ShiftL Source Int
    | ShiftR Source Int
