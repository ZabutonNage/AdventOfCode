module TwoB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String (split, Pattern(..))
import Data.Array (drop, head, mapMaybe, updateAt, (!!))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Control.Monad.Loops (iterateUntilM)


targetOutput :: Int
targetOutput = 19690720

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2019/_input/02.input"
  let program = mapMaybe fromString $ split (Pattern ",") input
  let maxAddr = addressBeforeHalt program

  let result = iterateUntilM
                 ((==) targetOutput <<< programResult program)
                 (next maxAddr)
                 (Tuple 0 0)
  case result of
    Nothing          -> log "error"
    Just (Tuple x y) -> log $ show $ 100 * x + y


next :: Int -> Tuple Int Int -> Maybe (Tuple Int Int)
next maxAddr (Tuple x y) | y == maxAddr = Nothing
                         | x == maxAddr = Just $ Tuple 0 (y+1)
                         | true         = Just $ Tuple (x+1) y

programResult :: Array Int -> Tuple Int Int -> Int
programResult arr (Tuple x y) = fromMaybe (-1) $ initProgram x y arr
                                                 >>= runProgram 0
                                                 >>= head

initProgram :: Int -> Int -> Array Int -> Maybe (Array Int)
initProgram x y arr = updateAt 1 x arr
                      >>= updateAt 2 y

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

addressBeforeHalt :: Array Int -> Int
addressBeforeHalt arr = countInstructions arr * 4 - 1

countInstructions :: Array Int -> Int
countInstructions = countInstr 0
  where
  countInstr acc arr = case head arr of
                         Just 99 -> acc
                         _       -> countInstr (acc + 1) (drop 4 arr)
