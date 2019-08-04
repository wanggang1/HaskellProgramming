module Cipher where

import Data.Char

cap1st :: [Char] -> [Char]
cap1st [] = []
cap1st (c:cs) = toUpper c : cs

cap :: [Char] -> [Char]
cap [] = []
cap (c:cs) = toUpper c : cap cs

capped1st :: [Char] -> Char
capped1st = toUpper . head

-- Caesar Cipher
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
