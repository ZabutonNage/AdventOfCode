module NineA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines)
import Data.List (List(..), (:), fromFoldable, catMaybes, take, drop, snoc, any)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)


preambleLength :: Int
preambleLength = 25


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/09.input"
  let nums     = catMaybes <<< fromFoldable $ fromString <$> lines input
  let preamble = take preambleLength nums
  let rest     = drop preambleLength nums
  let result   = solve preamble rest
  log $ show result



solve :: List Int -> List Int -> Maybe Int
solve pre@(_:xs) (y:ys) | isValid pre y = solve (snoc xs y) ys
                        | true          = Just y
solve _          _      = Nothing


isValid :: List Int -> Int -> Boolean
isValid Nil    _   = false
isValid (x:xs) num | any (eq num <<< add x) xs = true
                   | true                      = isValid xs num
