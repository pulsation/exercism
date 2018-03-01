module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
  | x < 1 = Nothing
  | x < aliquotSum x = Just Abundant
  | x == aliquotSum x = Just Perfect
  | x > aliquotSum x = Just Deficient

aliquotSum :: Int -> Int
aliquotSum x = sum $ divisorsExcludingItself x

divisorsExcludingItself :: Int -> [Int]
divisorsExcludingItself x = divisorsHelper x (x `div` 2)

divisorsHelper :: Int -> Int -> [Int]
divisorsHelper x y
  | y < 1 = []
  | y == 1 = [1]
  | x `mod` y == 0 = y : divisorsHelper x (y - 1)
  | otherwise = divisorsHelper x (y - 1)
