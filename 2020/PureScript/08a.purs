module EightA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines, words)
import Data.Array ((!!))
import Data.Set (Set, empty, insert, member)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/08.input"
  let instructions = parseInstruction <$> lines input
  let lastAcc = run instructions empty 0 0
  log $ show lastAcc



data Instruction
  = Nop
  | Acc Int
  | Jmp Int


parseInstruction :: String -> Instruction
parseInstruction raw = fromMaybe Nop $ do
  let parts = words raw
  param <- (parts !! 1) >>= fromString
  op    <- parts !! 0
  pure $ case op of
    "acc" -> Acc param
    "jmp" -> Jmp param
    _     -> Nop


run :: Array Instruction -> Set Int -> Int -> Int -> Int
run instructions visited i acc
  | i `member` visited = acc
  | true               = let newVisited = insert i visited
                             instr      = instructions !! i
                             nextRun    = run instructions newVisited
                         in case instr of
                           Just Nop     -> nextRun (i + 1) acc
                           Just (Acc x) -> nextRun (i + 1) (acc + x)
                           Just (Jmp x) -> nextRun (i + x) acc
                           Nothing      -> acc
