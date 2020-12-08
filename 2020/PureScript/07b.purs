module SevenB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (Pattern(..), split, joinWith)
import Data.String.Utils (lines, words)
import Data.Foldable (foldr)
import Data.Array ((!!), take, drop)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Int (fromString)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/07.input"
  let rules = foldr buildRules empty $ lines input
  log $ show $ requiredBags rules myBag



myBag :: String
myBag = "shiny gold"


type Rules = Map String (Array ContainedColor)
type ContainedColor = { count :: Int, color :: String }


buildRules :: String -> Rules -> Rules
buildRules raw rules = fromMaybe rules $ do
  let rawArr = split (Pattern " bags contain ") raw
  bag          <- rawArr !! 0
  rawContained <- rawArr !! 1
  let contained = parseContained rawContained
  pure $ insert bag contained rules


parseContained :: String -> Array ContainedColor
parseContained "no other bags." = []
parseContained raw              = do
  rawColor <- split (Pattern ", ") raw
  let colorParts = words rawColor
  pure $ buildColor colorParts


buildColor :: Array String -> ContainedColor
buildColor parts =
  let count = fromMaybe 0 <<< fromString <<< joinWith "" $ take 1 parts
      color = joinWith " " <<< take 2 $ drop 1 parts
  in { color, count }


requiredBags :: Rules -> String -> Int
requiredBags rules bag = fromMaybe 0 $ do
  nestedColors <- lookup bag rules
  pure $ foldr (\col cnt -> cnt + colorCount rules col) 0 nestedColors


colorCount :: Rules -> ContainedColor -> Int
colorCount rules concol = concol.count + concol.count * (fromMaybe 0 $ do
  nestedColors <- lookup concol.color rules
  let nestedCount = foldr (+) 0 $ colorCount rules <$> nestedColors
  pure nestedCount
  )
