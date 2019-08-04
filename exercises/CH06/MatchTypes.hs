import Data.List

data Blah = Blah deriving (Eq, Ord, Show)

freud :: Ord a => a -> a
freud x = x

jung :: Ord a => [a] -> a
jung xs = head (sort xs)

jung' :: [Int] -> Int
jung' = jung

young :: [Char] -> Char
young = jung

young' :: Ord a => [a] -> a
young' = jung

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn x y = fn x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn i x =  fromInteger i + fn x

{-Real -- (Integer, Int, Float, Double)-}
{-Fractional -- (Float, Double)-}
{-f :: Real a => a-}
f :: Fractional a => a
f = 1.0
