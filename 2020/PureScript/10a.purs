module TenA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines)
import Data.Array (sort, mapMaybe)
import Data.List (List, (:), fromFoldable)
import Data.Int (fromString)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/10.input"
  let outletJoltage = 0
  let adapters = outletJoltage : (fromFoldable $ sort $ mapMaybe fromString $ lines input)
  let ones   = diffs (\x y -> x + 1 == y) 0 adapters
  let threes = diffs (\x y -> x + 3 == y) 0 adapters + 1
  log $ show $ ones * threes



diffs :: forall a. (a -> a -> Boolean) -> Int -> List a -> Int
diffs pred acc (x:x1:xs) | pred x x1 = diffs pred (acc + 1) (x1:xs)
                         | true      = diffs pred acc (x1:xs)
diffs _    acc _         = acc
