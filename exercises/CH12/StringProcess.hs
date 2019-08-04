module CH12.StringProcess where

import Data.Char
import Data.List (intercalate)

notThe :: String -> Maybe String
notThe s = case (map toLower s) == "the" of
  True -> Nothing
  False -> Just s

replaceThe :: String -> String
replaceThe text = intercalate " " $ map replaceWithA $ map notThe (words text)

replaceWithA :: Maybe String -> String
replaceWithA Nothing = "a"
replaceWithA (Just s) = s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = findThes (words s) 0

vowels = "aeiou"

findThes :: [String] -> Integer -> Integer
findThes [] cnt = cnt
findThes (word:ws) cnt = case (map toLower word) == "the" of
  False -> findThes ws cnt
  True -> if ws == []
            then cnt
            else if elem (head $ head ws) vowels
              then findThes ws cnt+1
              else findThes ws cnt

countVowels :: String -> Int
countVowels s = length $ filter (\c -> elem c vowels) s

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord text =
  let numbV = countVowels text
      numbC = length text - numbV
   in case numbV - numbC > 0 of
        True -> Nothing
        False -> Just (Word' text)

