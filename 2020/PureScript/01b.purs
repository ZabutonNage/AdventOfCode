module OneB (main) where

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
  case productFromArrayRec1 a bs of
    Just p -> pure p
    Nothing -> productFromArray bs


productFromArrayRec1 :: Int -> Array Int -> Maybe Int
productFromArrayRec1 _ [] = Nothing
productFromArrayRec1 a bs = do
  { head: b, tail } <- uncons bs
  case productFromArrayRec2 a b tail of
    Just p -> pure p
    Nothing -> productFromArrayRec1 a tail


productFromArrayRec2 :: Int -> Int -> Array Int -> Maybe Int
productFromArrayRec2 _ _ [] = Nothing
productFromArrayRec2 a b cs = do
  { head: c, tail } <- uncons cs
  case productWhen2020 a b c of
    Just p -> pure p
    Nothing -> productFromArrayRec2 a b tail


productWhen2020 :: Int -> Int -> Int -> Maybe Int
productWhen2020 a b c | a + b + c == 2020 = Just (a * b * c)
                      | true              = Nothing
