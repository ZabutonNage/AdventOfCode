module OneA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines)
import Data.Array (uncons, mapMaybe)

import Data.Int (fromString)
import Data.Maybe (Maybe(..))


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/01.input"
  let ints = mapMaybe fromString (lines input)
  case productFromArray ints of
    Just solution -> log (show solution)
    Nothing -> log "Error"



productFromArray :: Array Int -> Maybe Int
productFromArray xs = do
  { head: a, tail: bs } <- uncons xs
  case productFromArrayRec a bs of
    Just p -> pure p
    Nothing -> productFromArray bs


productFromArrayRec :: Int -> Array Int -> Maybe Int
productFromArrayRec _ [] = Nothing
productFromArrayRec a bs = do
  { head: b, tail } <- uncons bs
  case productWhen2020 a b of
    Just p -> pure p
    Nothing -> productFromArrayRec a tail


productWhen2020 :: Int -> Int -> Maybe Int
productWhen2020 a b | a + b == 2020 = Just (a * b)
                    | true          = Nothing
