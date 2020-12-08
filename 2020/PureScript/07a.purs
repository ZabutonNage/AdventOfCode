module SevenA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (Pattern(..), split, joinWith)
import Data.String.Utils (lines, words)
import Data.Foldable (foldr)
import Data.Array ((!!), take, drop, length, nub)
import Data.Map (Map, empty, insertWith, lookup)
import Data.Maybe (fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/07.input"
  let colorRelations = foldr buildRules empty $ lines input
  let possibleContainers = nub $ containersForBag colorRelations myBag
  log $ show $ length possibleContainers



myBag :: String
myBag = "shiny gold"


type Rules = Map String (Array String)


buildRules :: String -> Rules -> Rules
buildRules raw rules = fromMaybe rules $ do
  let rawArr = split (Pattern " bags contain ") raw
  bag          <- rawArr !! 0
  rawContained <- rawArr !! 1
  let contained = parseContained rawContained
  pure $ foldr (addRule bag) rules contained


parseContained :: String -> Array String
parseContained "no other bags." = []
parseContained raw              = joinWith " " <<< take 2 <<< drop 1 <<< words <$> split (Pattern ", ") raw


addRule :: String -> String -> Rules -> Rules
addRule bag contained rules = insertWith (<>) contained [bag] rules


containersForBag :: Rules -> String -> Array String
containersForBag rules bag = fromMaybe [] $ do
  contained <- lookup bag rules
  pure $ contained <> foldr (\col cnt -> cnt <> containersForBag rules col) [] contained


colorCount :: String -> Rules -> Int
colorCount bag rules = fromMaybe 0 $ length <$> lookup bag rules
