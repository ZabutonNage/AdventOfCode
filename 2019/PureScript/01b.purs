module OneB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.List.Lazy.NonEmpty (iterate, tail)
import Data.List.Lazy (takeWhile)
import Data.Foldable (sum)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2019/_input/01.input"
  let totalFuel = sum $ (calcFuel <<< fromMaybe 0 <<< fromString) <$> lines input
  log $ show totalFuel


calcFuel :: Int -> Int
calcFuel mass = sum <<< takeWhile (_ > 0) <<< tail $ iterate fuel mass
  where
  fuel m = m / 3 - 2
