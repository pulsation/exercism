module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ sum $ zipWith mutation xs ys

mutation :: Char -> Char -> Int
mutation x y
  | x /= y = 1
  | otherwise = 0
