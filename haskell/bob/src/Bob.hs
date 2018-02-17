module Bob (responseFor) where

import Data.Char (isSpace)

responseFor :: String -> String
responseFor xs
  | silence xs = "Fine. Be that way!"
  | yellQuestion xs = "Calm down, I know what I'm doing!"
  | question xs = "Sure."
  | yell xs = "Whoa, chill out!"
  | otherwise = "Whatever."

question :: String -> Bool
question xs = last (trim xs) == '?'

yell :: String -> Bool
yell xs = not (null letters) && all(`elem` upperCaseLetters) letters
  where letters = allLetters xs

yellQuestion :: String -> Bool
yellQuestion xs = question xs && yell xs

silence :: String -> Bool
silence xs = trim xs == ""

upperCaseLetters :: String
upperCaseLetters = ['A'..'Z']
lowerCaseLetters :: String
lowerCaseLetters = ['a'..'z']

allLetters :: String -> String
allLetters = filter (`elem` upperCaseLetters ++ lowerCaseLetters)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
