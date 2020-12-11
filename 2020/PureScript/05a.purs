module FiveA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (Pattern(..), split, joinWith, take, drop)
import Data.String.Utils (lines)
import Data.Foldable (foldl)
import Data.Int (fromStringAs, binary)
import Data.Maybe (fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/05.input"
  let highestSeatId = foldl max 0 $ seatIdFromRaw <$> lines input
  log $ show highestSeatId



seatIdFromRaw :: String -> Int
seatIdFromRaw encodedSeat =
  let row = decodeBinary (take 7 encodedSeat)
      col = decodeBinary (drop 7 encodedSeat)
  in seatId row col


decodeBinary :: String -> Int
decodeBinary encodedSeat =
  let code = split (Pattern "") encodedSeat
      bits = toBit <$> code
  in fromMaybe 0 <<< fromStringAs binary <<< joinWith "" $ bits


toBit :: String -> String
toBit "B" = "1"
toBit "R" = "1"
toBit _   = "0"


seatId :: Int -> Int -> Int
seatId row col = row * 8 + col
