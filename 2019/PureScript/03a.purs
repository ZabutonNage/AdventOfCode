module ThreeA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, splitAt, Pattern(..))
import Data.String.Utils (lines)
import Data.Sequence as Seq
import Data.Sequence.NonEmpty as NonEmpty
import Data.Foldable (foldl, minimum)
import Data.Int (fromString)
import Data.Ord (abs)
import Data.Maybe (Maybe(..), fromMaybe)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2019/_input/03.input"
  log $ show $ solve input


solve :: String -> Maybe Int
solve raw = lines raw <#> split (Pattern ",")
                      <#> (_ <#> (splitAt 1 >>> toMove))
                      <#> connections
                      # closestManhattanDist

toMove :: Split -> Move
toMove { before, after } = { dir: before
                           , dist: fromMaybe 0 (fromString after) }

connections :: Array Move -> Seq.Seq Connection
connections moves = NonEmpty.tail $ foldl folder (NonEmpty.singleton { a: { x: 0, y: 0 }, b: { x: 0, y: 0 } }) moves
  where
  folder seq move = let { a, b: prev } = NonEmpty.last seq
                        next           = doMove prev move
                    in seq `NonEmpty.snoc` { a: prev, b: next }
  doMove { x, y } { dir: "U", dist } = { x: x, y: y + dist }
  doMove { x, y } { dir: "R", dist } = { x: x + dist, y: y }
  doMove { x, y } { dir: "D", dist } = { x: x, y: y - dist }
  doMove { x, y } { dir: "L", dist } = { x: x - dist, y: y }
  doMove _        _                  = { x: -1, y: -1 }

closestManhattanDist :: Array (Seq.Seq Connection) -> Maybe Int
closestManhattanDist [xs, ys] = mapMaybeConcat (\x -> crossing x <$> ys) xs
                                <#> manhattanDist
                                # Seq.filter (_ > 0)
                                # minimum
closestManhattanDist _        = Nothing

mapMaybeConcat :: (Connection -> Seq.Seq (Maybe XY)) -> Seq.Seq Connection -> Seq.Seq XY
mapMaybeConcat f seq = foldl folder Seq.empty seq
  where
  folder seq_ c = foldl folder2 seq_ (f c)
  folder2 seq_ Nothing   = seq_
  folder2 seq_ (Just xy) = seq_ `Seq.snoc` xy

manhattanDist :: XY -> Int
manhattanDist { x, y } = abs x + abs y

crossing :: Connection -> Connection -> Maybe XY
crossing c1 c2
  | vertical c1   && vertical c2   = Nothing
  | horizontal c1 && horizontal c2 = Nothing
  | vertical c1 && between c1.a.y c1.b.y c2.a.y && between c2.a.x c2.b.x c1.a.x   = Just { x: c1.a.x, y: c2.a.y }
  | horizontal c1 && between c1.a.x c1.b.x c2.a.x && between c2.a.y c2.b.y c1.a.y = Just { x: c2.a.x, y: c1.a.y }
  | true = Nothing

vertical :: Connection -> Boolean
vertical { a, b } = a.x == b.x

horizontal :: Connection -> Boolean
horizontal { a, b } = a.y == b.y


type Split = { before :: String, after :: String }
type Move  = { dir :: String, dist :: Int }
type XY    = { x :: Int, y :: Int }
type Connection = { a :: XY, b :: XY }
