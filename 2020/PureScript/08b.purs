module EightB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.String.Utils (lines, words)
import Data.Array ((!!), findMap, updateAt, catMaybes, mapWithIndex)
import Data.Set (Set, empty, insert, member)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2020/_input/08.input"
  let instructions = possibleFixes $ parseInstruction <$> lines input
  let result = findMap (\instr -> run instr empty 0 0) instructions
  log $ show result



run :: Array Instruction -> Set Int -> Int -> Int -> Maybe Int
run instructions visited i acc
  | i `member` visited = Nothing
  | true               = let newVisited = insert i visited
                             instr      = instructions !! i
                             nextRun    = run instructions newVisited
                         in case instr of
                           Just (Acc x) -> nextRun (i + 1) (acc + x)
                           Just (Jmp x) -> nextRun (i + x) acc
                           Just _       -> nextRun (i + 1) acc
                           Nothing      -> Just acc


possibleFixes :: Array Instruction -> Array (Array Instruction)
possibleFixes instructions = catMaybes $ mapWithIndex tryFix instructions
  where
  tryFix i (Nop x) = updateAt i (Jmp x) instructions
  tryFix i (Jmp x) = updateAt i (Nop x) instructions
  tryFix _ _       = Nothing


data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int


parseInstruction :: String -> Instruction
parseInstruction raw = fromMaybe (Nop 0) $ do
  let parts = words raw
  param <- (parts !! 1) >>= fromString
  op    <- parts !! 0
  pure $ case op of
    "acc" -> Acc param
    "jmp" -> Jmp param
    _     -> Nop param


flipOp :: Instruction -> Instruction
flipOp (Nop x) = Jmp x
flipOp (Jmp x) = Nop x
flipOp instr   = instr
