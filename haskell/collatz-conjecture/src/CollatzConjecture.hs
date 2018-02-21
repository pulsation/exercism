module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatz' n 0

collatz' :: Integer -> Integer -> Maybe Integer
collatz' n step
  | n < 1 = Nothing
  | n == 1 = Just step
  | even n = collatz' (n `quot` 2) oneMoreStep
  | otherwise = collatz' (n * 3 + 1) oneMoreStep
  where oneMoreStep = step + 1
