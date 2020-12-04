module ThreeB (main) where

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
import Data.Foldable (product)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/03.input"
  let treesPerSlope = encounteredTrees (lines input) <$> slopes
  let solution = product treesPerSlope
  log $ show solution



type Slope =
  { x :: Int
  , y :: Int
  }

slopes :: Array Slope
slopes =
  [ { x: 1, y: 1 }
  , { x: 3, y: 1 }
  , { x: 5, y: 1 }
  , { x: 7, y: 1 }
  , { x: 1, y: 2 }
  ]


encounteredTrees :: Array String -> Slope -> Number
encounteredTrees rows slope = iterator 0 0 0.0
  where
    rowCount = length rows
    iterator x y treeCount | y >= rowCount   = treeCount
                           | isTree x y rows = iterator (x + slope.x) (y + slope.y) (treeCount + 1.0)
                           | true            = iterator (x + slope.x) (y + slope.y) treeCount


isTree :: Int -> Int -> Array String -> Boolean
isTree x y rows = fromMaybe false $ do
  row <- rows !! y
  let i = x `rem` (Str.length row)
  char <- charAt i row
  pure (char == "#")
