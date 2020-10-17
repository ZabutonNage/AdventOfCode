module ThirteenA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines, words, stripChars)
import Data.Array (index, range, insertAt, length, snoc, reverse, mapMaybe)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Foldable (foldr)
import Data.Map (Map, insert, alter, singleton, empty, keys, lookup)
import Data.Set (toUnfoldable)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2015/_input/13.input"
  let guests       = guestsFromPreferences $ parseSeatingPreference <$> lines input
      arrangements = permutations $ toUnfoldable (keys guests)
      highscore    = foldr max 0 $ seatingHappiness guests <$> arrangements

  log $ show highscore


type GuestsWithNeighbours = Map String (Map String Int)

type Guest =
  { name :: String
  , neighbours :: Map String Int
  }

type SeatingPreference =
  { name :: String
  , happiness :: Int
  , neighbour :: String
  }


guestsFromPreferences :: Array SeatingPreference -> GuestsWithNeighbours
guestsFromPreferences preferences = foldr reduce empty preferences
  where
  reduce pref guests = alter (case _ of Nothing         -> Just $ singleton pref.neighbour pref.happiness
                                        Just neighbours -> Just $ insert pref.neighbour pref.happiness neighbours
                             ) pref.name guests

parseSeatingPreference :: String -> SeatingPreference
parseSeatingPreference raw = let parts        = words $ stripChars "." raw
                                 getFromParts = getOrEmpty parts
                                 signature    = if getFromParts 2 == "gain" then 1 else -1
                             in { name     : getFromParts 0
                                , happiness: (*) signature <<< fromMaybe 0 <<< fromString $ getFromParts 3
                                , neighbour: getFromParts 10
                                }

seatingHappiness :: GuestsWithNeighbours -> Array String -> Int
seatingHappiness guestMap names =
  let circledNames = snoc names $ getOrEmpty names 0
      reversed     = reverse circledNames
      scoreR       = foldr reduce Nothing circledNames
      scoreL       = foldr reduce Nothing reversed
  in fromMaybe 0 $ do
    r <- scoreR
    l <- scoreL
    Just (r.score + l.score)
  where
  reduce :: String -> Maybe { neighbours :: Maybe (Map String Int), score :: Int } -> Maybe { neighbours :: Maybe (Map String Int), score :: Int }
  reduce name Nothing                      = Just { neighbours: lookup name guestMap, score: 0 }
  reduce name (Just { neighbours, score }) = Just { neighbours: lookup name guestMap
                                                  , score: score + fromMaybe 0 (neighbours >>= lookup name)
                                                  }


getOrEmpty :: Array String -> Int -> String
getOrEmpty arr i = fromMaybe "" (index arr i)

permutations :: Array String -> Array (Array String)
permutations strings = do
  ixs <- indexPermutations (length strings)
  pure $ mapMaybe (index strings) ixs

indexPermutations :: Int -> Array (Array Int)
indexPermutations n | n <= 0 = []
indexPermutations 1   = [[0]]
indexPermutations n = do
  p <- indexPermutations (n - 1)
  i <- range 0 (n - 1)
  maybe [] pure $ insertAt i (n - 1) p
