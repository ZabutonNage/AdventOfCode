module FiveA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, Pattern(..), take, drop)
import Data.String.Utils (lines)
import Data.Foldable (foldl)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/05.input"
  let highestSeatId = foldl max 0 $ seatIdFromRaw <$> lines input
  log $ show highestSeatId



type SeatRange = { lower :: Int, upper :: Int }


seatIdFromRaw :: String -> Int
seatIdFromRaw encodedSeat =
  let row = seatRow encodedSeat
      col = seatCol encodedSeat
  in seatId row col


seatRow :: String -> Int
seatRow encodedSeat =
  let def  = { lower: 0, upper: 127 }
      code = split (Pattern "") (take 7 encodedSeat)
      singletonRng = foldl folder def code
  in singletonRng.lower
  where
  folder rng "F" = lowerRange rng
  folder rng _   = upperRange rng


seatCol :: String -> Int
seatCol encodedSeat =
  let def  = { lower: 0, upper: 7 }
      code = split (Pattern "") (drop 7 encodedSeat)
      singletonRng = foldl folder def code
  in singletonRng.lower
  where
  folder rng "L" = lowerRange rng
  folder rng _   = upperRange rng


upperRange :: SeatRange -> SeatRange
upperRange seatRange = { lower: rangeSize seatRange / 2 + seatRange.lower
                       , upper: seatRange.upper
                       }

lowerRange :: SeatRange -> SeatRange
lowerRange seatRange = { lower: seatRange.lower
                       , upper: rangeSize seatRange / 2 + seatRange.lower - 1
                       }

rangeSize :: SeatRange -> Int
rangeSize rng = rng.upper - rng.lower + 1

seatId :: Int -> Int -> Int
seatId row col = row * 8 + col
