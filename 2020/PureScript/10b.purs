module TenB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines)
import Data.Array (sort, mapMaybe, last)
import Data.List (List(..), (:), filter, foldr, fromFoldable, length, reverse, snoc)
import Data.Number (fromString)
import Data.Maybe (fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/10.input"
  let outletJoltage = 0.0
  let adapters = sort $ mapMaybe fromString $ lines input
  let device = fromMaybe 0.0 $ add 3.0 <$> last adapters

  let splitSequences = splitBy3Step $ outletJoltage : (fromFoldable adapters) `snoc` device
  let variableSeqs = filter ((<) 2 <<< length) splitSequences
  let combinationsPerSeq = countValid <$> variableSeqs
  let combinations = foldr (*) 1.0 combinationsPerSeq

  log $ show combinations



splitBy3Step :: List Number -> List (List Number)
splitBy3Step list = reverse <$> subseqs Nil Nil list
  where
  subseqs result acc (x:x1:xs) | x1 - x < 3.0 = subseqs result (x:acc) (x1:xs)
                               | true       = subseqs ((x:acc):result) Nil (x1:xs)
  subseqs result acc xs = xs:result


countValid :: List Number -> Number
countValid Nil        = 0.0
countValid (first:xs) = cv first xs
  where
  cv prev (y:Nil)     = if y - prev <= 3.0 then 1.0 else 0.0
  cv prev (y:ys)
    | y - prev <= 3.0 = (cv y ys) + (cv prev ys)
  cv _    _           = 0.0
