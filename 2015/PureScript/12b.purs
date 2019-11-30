module TwelveB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))

import Data.Argonaut.Core (Json, caseJson, caseJsonString)
import Data.Argonaut.Parser (jsonParser)
import Foreign.Object as FO
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
                    (\obj -> if hasRed obj
                               then 0
                               else foldr ((+) <<< sumAllNums) 0 obj)
                    json

hasRed :: FO.Object Json -> Boolean
hasRed obj = not $ FO.all (\_ v -> caseJsonString true (_ /= "red") v) obj
