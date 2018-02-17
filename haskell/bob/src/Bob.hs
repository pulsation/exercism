module Bob (responseFor) where

import Data.Char (isSpace)

responseFor :: String -> String
responseFor xs
  | hush xs = "Fine. Be that way!"
  | yellQuestion xs = "Calm down, I know what I'm doing!"
  | ask xs = "Sure."
  | yell xs = "Whoa, chill out!"
  | otherwise = "Whatever."

upperCaseLetters = ['A'..'Z']
lowerCaseLetters = ['a'..'z']

allLetters :: String -> String
allLetters xs = filter (`elem` upperCaseLetters ++ lowerCaseLetters) xs

ask :: String -> Bool
ask xs = (last (trim xs) == '?')

yell :: String -> Bool
yell xs = all(`elem` upperCaseLetters) (allLetters xs)

yellQuestion :: String -> Bool
yellQuestion xs = ask xs && yell xs

hush :: String -> Bool
hush xs = trim (allLetters xs) == ""

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
