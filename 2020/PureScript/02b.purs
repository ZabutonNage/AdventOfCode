module TwoB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, Pattern(..), take)
import Data.String.Utils (lines, words, charAt)
import Data.Array (mapMaybe, (!!), filter, length)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/02.input"
  let validEntries = length $ filter (validateEntry <<< parseEntry) (lines input)
  log $ show validEntries



type Entry =
  { pos1 :: Int
  , pos2 :: Int
  , char :: String
  , pwd :: String
  }


validateEntry :: Entry -> Boolean
validateEntry entry =
  let char1 = fromMaybe "" $ charAt (entry.pos1 - 1) entry.pwd
      char2 = fromMaybe "" $ charAt (entry.pos2 - 1) entry.pwd
  in (char1 == entry.char) /= (char2 == entry.char)


parseEntry :: String -> Entry
parseEntry str =
  let blocks = words str
      pos_ = fromMaybe [] $ split (Pattern "-") <$> (blocks !! 0)
      pos = mapMaybe fromString pos_
      pos1 = fromMaybe 0 (pos !! 0)
      pos2 = fromMaybe 0 (pos !! 1)
      char = fromMaybe "" $ take 1 <$> (blocks !! 1)
      pwd = fromMaybe "" (blocks !! 2)
  in { pos1
     , pos2
     , char
     , pwd
     }
