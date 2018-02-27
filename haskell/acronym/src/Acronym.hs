module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate phrase = concatMap (\word -> [toUpper $ head word]) $ separateWords phrase

separateWords :: String -> [String]
separateWords = filter (/= "") . foldl detectWord [""]

detectWord :: [String] -> Char -> [String]
detectWord wordAccumulator nextCharacter
  | isUpperLetterInsideWord = wordAccumulator ++ [[nextCharacter]]
  | isLetter nextCharacter = firstWords ++ [lastWord ++ [nextCharacter]]
  | otherwise = wordAccumulator ++ [""]
  where lastWord = last wordAccumulator
        firstWords = init wordAccumulator
        isUpperLetterInsideWord = isLetter nextCharacter
          && isUpper nextCharacter
          && not (null lastWord)
          && isLower (last lastWord)
