module ElevenB (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Data.String (split, Pattern(..))
import Data.String.Unsafe (char)
import Data.String.CodeUnits (singleton)
import Data.Char (toCharCode, fromCharCode)
import Data.List (List(..), foldr, fromFoldable, length, mapMaybe, notElem, reverse, (:))


main :: Effect Unit
main = do
  input <- readTextFile UTF8 "./2015/_input/11.input"
  log $ "in:  " <> input
  log $ "out: " <> nextValidPw (nextValidPw input)


nextValidPw :: String -> String
nextValidPw str = fromCharCodeList $ reverse $ getValid $ reverse $ toCharCodeList str
  where
  getValid Nil   = Nil
  getValid chars = let next = nextValidChars chars
                   in if isValid next
                        then next
                        else getValid next
  nextValidChars (x:xs) | x == maxCharCode = minCharCode : nextValidChars xs
                        | validChar (x +1) = (x+1):xs
                        | true             = nextValidChars $ (x+1):xs
  nextValidChars Nil    = Nil
  isValid chars = hasPairs chars && (hasSeq $ reverse chars)

toCharCodeList :: String -> List Int
toCharCodeList str = fromFoldable $ (toCharCode <<< char) <$> split (Pattern "") str

fromCharCodeList :: List Int -> String
fromCharCodeList chars = foldr (\c s -> singleton c <> s) "" $ mapMaybe fromCharCode chars

hasSeq :: List Int -> Boolean
hasSeq (x:y:z:xs) | length xs == 0 = false
                  | x + 1    == y
                    && y + 1 == z  = true
                  | true = hasSeq (y:z:xs)
hasSeq _          = false

invalidChars :: List Int
invalidChars = toCharCode <$> ('i':'o':'l':Nil)

minCharCode :: Int
minCharCode = toCharCode 'a'

maxCharCode :: Int
maxCharCode = toCharCode 'z'

validChar :: Int -> Boolean
validChar c = notElem c invalidChars

hasPairs :: List Int -> Boolean
hasPairs chars = hasPairs_ 0 chars
  where
  hasPairs_ 2     _        = true
  hasPairs_ pairs (x:y:xs) | x == y = hasPairs_ (pairs +1) xs
                           | true   = hasPairs_ pairs (y:xs)
  hasPairs_ _     _        = false
