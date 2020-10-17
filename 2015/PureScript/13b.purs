module ThirteenB (main) where

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
import Data.Map (Map, insert, alter, singleton, empty, keys, lookup, fromFoldable)
import Data.Set (toUnfoldable)
import Data.Tuple (Tuple(..))


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2015/_input/13.input"
  let guestsWithoutMe     = guestsFromPreferences $ parseSeatingPreference <$> lines input
      guestsWithMe        = addMe guestsWithoutMe
      arrangementsWithMe  = permutations $ toUnfoldable (keys guestsWithMe)
      happinessInfoWithMe = seatingHappiness guestsWithMe <$> arrangementsWithMe
      maxHappiness        = \a b -> if a.score > b.score then a else b
      highscoreWithMe     = foldr maxHappiness { score: 0, names: [] } happinessInfoWithMe

  log $ show highscoreWithMe



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

type HappinessInfo =
  { score :: Int
  , names :: Array String
  }


addMe :: GuestsWithNeighbours -> GuestsWithNeighbours
addMe guestMap = let names         = toUnfoldable $ keys guestMap :: Array String
                     withPrefForMe = guestMap <#> insert "Me" 0
                     myPrefs       = fromFoldable $ names <#> \name -> Tuple name 0
                 in insert "Me" myPrefs withPrefForMe

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

seatingHappiness :: GuestsWithNeighbours -> Array String -> HappinessInfo
seatingHappiness guestMap names =
  let circledNames = snoc names $ getOrEmpty names 0
      reversed     = reverse circledNames
      scoreR       = foldr reduce Nothing circledNames
      scoreL       = foldr reduce Nothing reversed
  in fromMaybe { score: 0, names: [] } $ do
    r <- scoreR
    l <- scoreL
    Just { score: r.score + l.score, names }
  where
  reduce :: String -> Maybe { neighbours :: Maybe (Map String Int), score :: Int } -> Maybe { neighbours :: Maybe (Map String Int), score :: Int }
  reduce name Nothing                      = Just { neighbours: lookup name guestMap, score: 0 }
  reduce name (Just { neighbours, score }) = Just { neighbours: lookup name guestMap
                                                  , score: score + fromMaybe 0 (neighbours >>= lookup name)
                                                  }


getOrEmpty :: Array String -> Int -> String
getOrEmpty arr i = fromMaybe "" (index arr i)

permutations :: forall a. Array a -> Array (Array a)
permutations arr = do
  ixs <- indexPermutations (length arr)
  pure $ mapMaybe (index arr) ixs

indexPermutations :: Int -> Array (Array Int)
indexPermutations n | n <= 0 = []
indexPermutations 1   = [[0]]
indexPermutations n = do
  p <- indexPermutations (n - 1)
  i <- range 0 (n - 1)
  maybe [] pure $ insertAt i (n - 1) p

insertAtEach :: forall a. Array a -> a -> Array (Array a)
insertAtEach arr x = mapMaybe
                       (\i -> insertAt i x arr)
                       (range 0 (length arr - 1))
