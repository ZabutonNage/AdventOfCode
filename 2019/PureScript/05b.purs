module FiveB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.Int (fromString, pow)
import Data.String (split, Pattern(..))
import Data.Array ((!!), updateAt, snoc)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

input :: Int
input = 5


main :: Effect Unit
main = do
  raw <- readTextFile UTF8 "./2019/_input/05.input"

  case readRawInput raw >>= runProgram 0 []
  of
    Nothing   -> log "Error"
    Just outp -> void $ traverse (log <<< show) outp


runProgram :: Int -> Output -> Instructions -> Maybe Output
runProgram i outp instr = do
  code <- instr !! i

  let getVal      = \pos    -> getParamMode code pos >>= getValue instr (i + pos)
      updateInstr = \ix val -> updateAt ix val instr

  case getOpcode code of
    -- add r r w
    1  -> runProgram (i+4) outp =<< do
                                      v1 <- getVal 1
                                      v2 <- getVal 2
                                      at <- instr !! (i+3)
                                      updateInstr at (v1 + v2)
    -- multiply r r w
    2  -> runProgram (i+4) outp =<< do
                                      v1 <- getVal 1
                                      v2 <- getVal 2
                                      at <- instr !! (i+3)
                                      updateInstr at (v1 * v2)
    -- store input w
    3  -> runProgram (i+2) outp =<< do
                                      at <- instr !! (i+1)
                                      updateInstr at input
    -- output r
    4  -> flip (runProgram (i+2)) instr =<< (snoc outp <$> getVal 1)
    -- jump-if-true r r
    5  -> do
            nextI <- case getVal 1 of
                       Just 0 -> pure (i+3)
                       _      -> getVal 2
            runProgram nextI outp instr
    -- jump-if-false r r
    6  -> do
            nextI <- case getVal 1 of
                       Just 0 -> getVal 2
                       _      -> pure (i+3)
            runProgram nextI outp instr
    -- less than r r w
    7  -> runProgram (i+4) outp =<< do
                                      v1 <- getVal 1
                                      v2 <- getVal 2
                                      let v3 = if (v1 < v2) then 1 else 0
                                      at <- instr !! (i+3)
                                      updateInstr at v3
    -- equals r r w
    8  -> runProgram (i+4) outp =<< do
                                      v1 <- getVal 1
                                      v2 <- getVal 2
                                      let v3 = if (v1 == v2) then 1 else 0
                                      at <- instr !! (i+3)
                                      updateInstr at v3
    -- halt
    99 -> Just outp
    -- error
    _  -> Nothing


getOpcode :: Int -> Int
getOpcode code = code `mod` 100

getParamMode :: Int -> Int -> Maybe ParameterMode
getParamMode code pos = toParamMode $ (code / (100 * pow 10 (pos - 1))) `mod` (pow 10 pos)

toParamMode :: Int -> Maybe ParameterMode
toParamMode 0 = Just Position
toParamMode 1 = Just Immediate
toParamMode _ = Nothing

getValue :: Instructions -> Int -> ParameterMode -> Maybe Int
getValue instr i Position  = instr !! i >>= (instr !! _)
getValue instr i Immediate = instr !! i

readRawInput :: String -> Maybe Instructions
readRawInput raw = traverse fromString $ split (Pattern ",") raw


type Instructions = Array Int
type Output       = Array Int

data ParameterMode = Position
                   | Immediate
