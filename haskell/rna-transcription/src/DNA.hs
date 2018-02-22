module DNA (toRNA) where

import Data.Map

toRNA :: String -> Maybe String
toRNA [] = Just ""
toRNA (x:xs) = Data.Map.lookup x complement >>= replaceFirstNucleotide xs
  where replaceFirstNucleotide ns n = fmap (n++)(toRNA ns)
        complement = Data.Map.fromList [('G', "C"), ('C', "G"), ('T', "A"), ('A', "U")]
