module NineB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines)
import Data.List (List(..), (:), foldr, fromFoldable, catMaybes, take, drop, snoc, any, reverse, length)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Int (fromString, floor, toNumber)
import Data.Number (infinity)


preambleLength :: Int
preambleLength = 25


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/09.input"
  let nums     = catMaybes <<< fromFoldable $ fromString <$> lines input
  let preamble = take preambleLength nums
  let rest     = drop preambleLength nums
  let weakness = findInvalid preamble rest >>= findWeakness nums
  log $ show weakness



findWeakness :: List Int -> Int -> Maybe Int
findWeakness Nil         _          = Nothing
findWeakness nums@(_:xs) invalidNum =
  let range      = toNumber <$> takeWhileAccumulator taker 0 nums
      isWeakness = length range > 1 && sum range == toNumber invalidNum
  in if isWeakness
     then Just $ floor (sumExtremes range)
     else findWeakness xs invalidNum
  where
  taker acc y | (acc + y) <= invalidNum = Just (acc + y)
              | true                    = Nothing


findInvalid :: List Int -> List Int -> Maybe Int
findInvalid pre@(_:xs) (y:ys) | isValid pre y = findInvalid (snoc xs y) ys
                              | true          = Just y
findInvalid _          _      = Nothing


isValid :: List Int -> Int -> Boolean
isValid Nil    _   = false
isValid (x:xs) num | any (eq num <<< add x) xs = true
                   | true                      = isValid xs num


takeWhileAccumulator :: forall a. (a -> a -> Maybe a) -> a -> List a -> List a
takeWhileAccumulator pred acc list = reverse $ twa acc list Nil
  where
  twa _  Nil    res = res
  twa ac (x:xs) res = case pred ac x of
                        Just nextAcc -> twa nextAcc xs (x:res)
                        Nothing      -> res


sumExtremes :: List Number -> Number
sumExtremes list = (foldr min infinity list) + (foldr max (-infinity) list)
