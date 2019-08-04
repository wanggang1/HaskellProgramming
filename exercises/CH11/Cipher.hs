module CH11.Cipher where

import Data.Char
import Data.List

------- Vigenere Cipher
-- NOT working, because the key word matching restarted for each word
vigenere :: [Char] -> [Char] -> [Char]
vigenere _ [] = []
vigenere [] _ = []
vigenere ks cs = intercalate " " [vigenere' ks word | word <- words cs]

-- this ONLY works with word, NOT sentence with space between words
vigenere' :: [Char] -> [Char] -> [Char]
vigenere' ks cs =
  concat [caesar i [c] | (i, c) <- zip (shifter (length cs) ks) cs]

-- Not implemented, yet
unVigenere :: [Char] -> [Char] -> [Char]
unVigenere _ [] = []
unVigenere [] _ = []
unVigenere ks cs = unCaesar 0 cs

shiftBy :: Char -> Int
shiftBy c =
  if isLowerAt c'
     then c' - 97 + 26
     else if isUpperAt c'
         then c' - 65
         else 0
  where c' = ord c

shifter :: Int -> [Char] -> [Int]
shifter l ks =
  if length ks >= l
     then map shiftBy (take l ks)
     else
         shifter l (ks ++ ks)


------- Caesar Cipher
caesar :: Int -> [Char] -> [Char]
caesar i [] = []
caesar i (c:cs) = chr (rightShift i (ord c)) : caesar i cs

unCaesar :: Int -> [Char] -> [Char]
unCaesar i [] = []
unCaesar i (c:cs) = chr (leftShift i (ord c)) : unCaesar i cs

rightShift :: Int -> Int -> Int
rightShift i p =
  if isLowerAt p
     then
       if p' <= 122 then p' else p' - 26
     else if isUpperAt p
       then
         if  p' <= 90 then p' else p' - 26
       else p
  where p' = p + (i `mod` 26)

leftShift :: Int -> Int -> Int
leftShift i p =
  if isLowerAt p
     then
       if p' >= 97 then p' else p' + 26
     else if isUpperAt p
       then
         if  p' >= 65 then p' else p' + 26
       else p
  where p' = p - (i `mod` 26)

isLowerAt :: Int -> Bool
isLowerAt i = i > 96 && i < 123

isUpperAt :: Int -> Bool
isUpperAt i = i > 64 && i < 91
