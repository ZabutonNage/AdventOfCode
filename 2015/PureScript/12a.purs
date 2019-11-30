module TwelveA (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.Argonaut.Core (Json, caseJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Array (foldr)
import Data.Int (floor)


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2015/_input/12.input"
  case jsonParser input of
    Left  msg  -> log msg
    Right json -> log $ show $ sumAllNums json


sumAllNums :: Json -> Int
sumAllNums json = caseJson
                    (const 0)
                    (const 0)
                    (floor)
                    (const 0)
                    (foldr ((+) <<< sumAllNums) 0)
                    (foldr ((+) <<< sumAllNums) 0)
                    json
