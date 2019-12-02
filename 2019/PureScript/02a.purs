module TwoA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, Pattern(..))
import Data.Array (updateAt, mapMaybe, head, (!!))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2019/_input/02.input"
  let program = mapMaybe fromString $ split (Pattern ",") input
  let processed = initProgram program >>= runProgram 0 >>= head
  case processed of
    Just result -> log $ show result
    Nothing     -> log "error"


initProgram :: Array Int -> Maybe (Array Int)
initProgram arr = updateAt 1 12 arr
                  >>= updateAt 2 2

runProgram :: Int -> Array Int -> Maybe (Array Int)
runProgram i arr = case arr !! i of
                  Just 1  -> runOpcode (+) i arr >>= runProgram (i + 4)
                  Just 2  -> runOpcode (*) i arr >>= runProgram (i + 4)
                  Just 99 -> Just arr
                  _       -> Nothing

runOpcode :: (Int -> Int -> Int) -> Int -> Array Int -> Maybe (Array Int)
runOpcode f i arr = do
  p1 <- arr !! (i + 1)
  p2 <- arr !! (i + 2)
  p3 <- arr !! (i + 3)
  v1 <- arr !! p1
  v2 <- arr !! p2
  updateAt p3 (f v1 v2) arr
