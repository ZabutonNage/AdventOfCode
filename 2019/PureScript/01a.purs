module OneA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines)
import Data.Array (foldr)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2019/_input/01.input"
  let totalFuel = foldr (+) 0 $ (calcFuel <<< fromMaybe 0 <<< fromString) <$> lines input
  log $ show totalFuel


calcFuel :: Int -> Int
calcFuel m = m / 3 - 2
