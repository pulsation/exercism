module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = numberOfFoundAlphabetLetters >= 26
  where foundAlphabetLetters = findAlphabetLettersIn text []
        numberOfFoundAlphabetLetters = length foundAlphabetLetters

findAlphabetLettersIn :: String -> String -> String
findAlphabetLettersIn [] foundLetters = foundLetters
findAlphabetLettersIn (letter:letters) foundLetters =
  findAlphabetLettersIn letters (addLetter lowerCaseLetter foundLetters)
  where lowerCaseLetter = toLower letter
        addLetter currentLetter alreadyFoundLetters
          | currentLetter `elem` alreadyFoundLetters = alreadyFoundLetters
          | currentLetter `elem` ['a'..'z'] = alreadyFoundLetters ++ [currentLetter]
          | otherwise = alreadyFoundLetters
