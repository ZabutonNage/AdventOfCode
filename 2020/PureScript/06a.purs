module SixA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, replaceAll, Pattern(..), Replacement(..))
import Data.Foldable (sum)
import Data.Array (length, nub)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/06.input"
  let allYeses = sum $ yesesInGroup <$> parseGroups input
  log $ show allYeses



type Group = String

parseGroups :: String -> Array Group
parseGroups raw = replaceAll (Pattern "\n") (Replacement "") <$> split (Pattern "\n\n") raw

yesesInGroup :: Group -> Int
yesesInGroup group = length $ nub $ split (Pattern "") group
