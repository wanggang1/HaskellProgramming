module HP.Ch05 where

chapter05 :: IO ()
chapter05 = putStrLn "Chapter 05 - Types"

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

---------- Curry --------------
curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)

----------- Uncurried ------------
uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)

----------- Partial Application, can only apply in order of the parameters in list ----------
base100 = curriedFunction 100

has10 :: [Integer] -> Bool
has10 = elem 10

---------- Section, partial application for binary operators only  ----------------------
sect100 = (100 `curriedFunction`)
sectTrue = (`curriedFunction` True)

contains = (`elem` [1..10] )

hasTen :: [Integer] -> Bool
hasTen = (10 `elem`)

------------ Type Inference -------------------------------
f :: Num a => a -> a -> a
f x y = x + y + 3

f2 x y = x + y + 3

