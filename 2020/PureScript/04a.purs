module FourA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, Pattern(..))
import Data.String.Regex (regex, split) as R
import Data.String.Regex.Flags (global)
import Data.Array ((!!), filter, length)
import Data.Foldable (all, elem)
import Data.Maybe (fromMaybe)
import Data.Either (Either(..))


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/04.input"
  let validCount = validPassports input
  log $ show validCount



requiredFields :: Array String
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


validPassports :: String -> Int
validPassports input =
  let keysPerPP = ppKeys (passports input)
  in length $ filter isValid keysPerPP
  where
  isValid keys = all (_ `elem` keys) requiredFields


passports :: String -> Array String
passports = split (Pattern "\n\n")


ppKeys :: Array String -> Array (Array String)
ppKeys ppBlocks = case (R.regex " |\\n" global) of
                    (Right rx) -> keysFromPairs <<< R.split rx <$> ppBlocks
                    (Left _)   -> []


keysFromPairs :: Array String -> Array String
keysFromPairs pairs = firstOr "" <<< split (Pattern ":") <$> pairs


firstOr :: forall a. a -> Array a -> a
firstOr def = fromMaybe def <<< (_ !! 0)
