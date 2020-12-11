module FiveB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (Pattern(..), split, joinWith, take, drop)
import Data.String.Utils (lines)
import Data.Array (sort)
import Data.List ((:), fromFoldable)
import Data.Int (fromStringAs, binary)
import Data.Maybe (Maybe(..), fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/05.input"
  let sortedIds = sort $ seatIdFromRaw <$> lines input
  log $ show (findGap sortedIds)



findGap :: Array Int -> Int
findGap nums = findGap_ Nothing $ fromFoldable nums
  where
  findGap_ Nothing     (x:xs) = findGap_ (Just x) xs
  findGap_ (Just prev) (x:xs) | x > prev + 1 = prev + 1
                              | true         = findGap_ (Just x) xs
  findGap_ _           _      = 0


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
