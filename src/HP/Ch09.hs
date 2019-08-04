module HP.Ch09 where

import Data.Char (isSpace)

chapter09 :: IO ()
chapter09 = putStrLn "Chapter 09 - Lists"

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x : _) = Just x

--enumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False True = [False, True]
eftBool True True = [True]
eftBool False False = [False]

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | x == y = [x]
  | otherwise = eftInt x (y - 1) ++ [y]

-- List comprehension
acro :: String -> String
acro words = [c | c <- words, elem c ['A'..'Z']]

-- map
{-import Data.Bool-}
{-map (\x -> bool x 0 (x < 0)) [-5..5]-}

-- filter
multOf3 :: [Int] -> [Int]
multOf3 xs = filter (\x -> (rem x 3) == 0) xs

count3s :: [Int] -> Int
count3s = length . multOf3

myWords :: String -> [String]
myWords s =  case dropWhile isSpace s of
                "" -> []
                s' -> w : myWords s''
                      where (w, s'') = break isSpace s'

notArticles :: String -> Bool
notArticles = not . (`elem` ["the", "a", "an"])

myFilter :: String -> [String]
myFilter = (filter notArticles) . myWords

-- zip
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip1 :: [a] -> [b] -> [(a, b)]
myZip1 = myZipWith (,)
{-myZip1 = myZipWith (\x y -> (x, y))-}

