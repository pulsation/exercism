module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs =
  let allNucleotideCounts = countNucleotides ['A', 'C', 'G', 'T'] xs
  in fmap fromList (sequenceA allNucleotideCounts)

countNucleotides :: String -> String -> [Either String (Char, Int)]
countNucleotides [] extraCharacters
  | null extraCharacters = []
  | otherwise = [ Left ("Extra characters found in DNA sequence: "
                  ++ extraCharacters) ]
countNucleotides (nucleotide:nucleotides) dnaSequence =
  let remainingDnaSequence = filter (/= nucleotide) dnaSequence
  in Right (nucleotide, countNucleotides' nucleotide dnaSequence)
  : countNucleotides nucleotides remainingDnaSequence

countNucleotides' :: Char -> String -> Int
countNucleotides' nucleotide dnaSequence =
  length $ filter ( == nucleotide) dnaSequence
