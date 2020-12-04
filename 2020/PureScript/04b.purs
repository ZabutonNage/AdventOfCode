module FourB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, Pattern(..))
import Data.String.Regex (regex, split, test) as R
import Data.String.Regex.Flags (global, noFlags)
import Data.Array (filter, length)
import Data.Foldable (all, any, elem)
import Data.Either (Either(..))


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/04.input"
  let validCount = validPassports input
  log $ show validCount



requiredFields :: Array Constraint
requiredFields =
  [ { k: "byr", valid: validateRegex "^19[2-9]\\d|200[012]$" }
  , { k: "iyr", valid: validateRegex "^20(1\\d|20)$" }
  , { k: "eyr", valid: validateRegex "^20(2\\d|30)$" }
  , { k: "hgt", valid: validateRegex "^(1([5-8]\\d|9[0-3])cm|(59|6\\d|7[0-6])in)$" }
  , { k: "hcl", valid: validateRegex "^#[0-9a-f]{6}$" }
  , { k: "ecl", valid: (_ `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) }
  , { k: "pid", valid: validateRegex "^\\d{9}$" }
  ]


validateRegex :: String -> String -> Boolean
validateRegex pattern str = case (R.regex pattern noFlags) of
                              Left _   -> false
                              Right rx -> R.test rx str


type Constraint = { k :: String, valid :: String -> Boolean }
type KV = { k :: String, v :: String }


validPassports :: String -> Int
validPassports input =
  let kvPerPP = ppKVs (passports input)
  in length $ filter isValid kvPerPP
  where
  isValid kvs = all (\req -> any (\kv -> kv.k == req.k && req.valid kv.v) kvs) requiredFields


passports :: String -> Array String
passports = split (Pattern "\n\n")


ppKVs :: Array String -> Array (Array KV)
ppKVs ppBlocks = case (R.regex " |\\n" global) of
                   (Right rx) -> kvFromPairs <<< R.split rx <$> ppBlocks
                   (Left _)   -> []


kvFromPairs :: Array String -> Array KV
kvFromPairs pairs = toKV <<< split (Pattern ":") <$> pairs
  where
  toKV [k, v] = { k, v }
  toKV _      = { k: "", v: "" }
