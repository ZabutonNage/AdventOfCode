module FourA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (length, split, Pattern(..))
import Data.List (fromFoldable, (:), List(..), range)
import Data.Foldable (and, foldr)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2019/_input/04.input"
  case split (Pattern "-") input of
    [from, to] -> log $ show $ solve from to
    _          -> log "error"


solve :: String -> String -> Maybe Int
solve from to = do
  f <- fromString from
  t <- fromString to
  let validate = valid from to
  Just $ foldr (\num total -> if validate (show num) then total + 1 else total) 0 $ range f t

valid :: String -> String -> String -> Boolean
valid from to s = and $ [ sixDigits
                        , inRange from to
                        , hasPair
                        , ascending
                        ]
                        <@> s

sixDigits :: String -> Boolean
sixDigits s = length s == 6

inRange :: String -> String -> String -> Boolean
inRange from to s = s >= from && s <= to

hasPair :: String -> Boolean
hasPair s = hp $ fromFoldable $ split (Pattern "") s
  where
  hp Nil = false
  hp (x:Nil) = false
  hp (x:y:ys) | x == y = true
              | true   = hp (y:ys)

ascending :: String -> Boolean
ascending s = asc $ fromFoldable $ split (Pattern "") s
  where
  asc (x:y:ys) | x > y = false
               | true  = asc (y:ys)
  asc _        = true
