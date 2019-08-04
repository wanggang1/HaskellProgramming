module HP.Ch11 where

import Data.Char

chapter11 :: IO ()
chapter11 = putStrLn "Chapter 11 - Algebraic datatypes"

type Size = Integer

data Manufacturer = Mini | Mazda | Tata
                    deriving (Show, Eq)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
               deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
               deriving (Eq, Show)

myCar = Car Mini (Price 14000)
yourCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 30

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- as-patterns: pattern matching both partial and whole of a value
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:t) ys =
  if elem x ys
     then isSubsequenceOf t ys
     else False

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords sentence =
  map (\w@(c:cs) -> (w, (toUpper c):cs)) ws
    where ws = words sentence

