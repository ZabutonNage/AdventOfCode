module ThreeA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (length) as Str
import Data.String.Utils (lines, charAt)
import Data.Array ((!!), length)
import Data.Int (rem)
import Data.Maybe (fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/03.input"
  let solution = encounteredTrees (lines input)
  log $ show solution



encounteredTrees :: Array String -> Int
encounteredTrees rows = iterator 0 0 0
  where
    rowCount = length rows
    iterator x y treeCount | y >= rowCount   = treeCount
                           | isTree x y rows = iterator (x + 3) (y + 1) (treeCount + 1)
                           | true            = iterator (x + 3) (y + 1) treeCount


isTree :: Int -> Int -> Array String -> Boolean
isTree x y rows = fromMaybe false $ do
  row <- rows !! y
  let i = x `rem` (Str.length row)
  char <- charAt i row
  pure (char == "#")
