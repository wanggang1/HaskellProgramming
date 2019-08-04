module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = concat (intersperse "-" words)
  where words = map wordNumber (digits n)

digits :: Int -> [Int]
digits n
  | n < 1 = []
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"

