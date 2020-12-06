module SixB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, joinWith, Pattern(..))
import Data.Foldable (sum, length)
import Data.Array (filter, group')


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/06.input"
  let allYeses = sum $ yesesInGroup <$> parseGroups input
  log $ show allYeses



type Group = Array String

parseGroups :: String -> Array Group
parseGroups raw = split (Pattern "\n") <$> split (Pattern "\n\n") raw

yesesInGroup :: Group -> Int
yesesInGroup grp =
  let persons   = length grp :: Int
      groupedQs = grp
                  # joinWith ""
                  # split (Pattern "")
                  # group'
  in length $ filter ((==) persons <<< length) groupedQs
