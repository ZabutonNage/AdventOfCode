module TwoA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, Pattern(..), take, length)
import Data.String.Utils (lines, words, filter)
import Data.Array (mapMaybe, (!!))
import Data.Array (filter, length) as Arr
import Data.Int (fromString)
import Data.Maybe (fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/02.input"
  let validEntries = Arr.length $ Arr.filter (validateEntry <<< parseEntry) (lines input)
  log $ show validEntries



type Entry =
  { min :: Int
  , max :: Int
  , char :: String
  , pwd :: String
  }


validateEntry :: Entry -> Boolean
validateEntry entry =
  let charCount = length $ filter (_ == entry.char) entry.pwd
  in charCount >= entry.min && charCount <= entry.max


parseEntry :: String -> Entry
parseEntry str =
  let blocks = words str
      minMax_ = fromMaybe [] $ split (Pattern "-") <$> (blocks !! 0)
      minMax = mapMaybe fromString minMax_
      min = fromMaybe 0 (minMax !! 0)
      max = fromMaybe 0 (minMax !! 1)
      char = fromMaybe "" $ take 1 <$> (blocks !! 1)
      pwd = fromMaybe "" (blocks !! 2)
  in { min
     , max
     , char
     , pwd
     }
