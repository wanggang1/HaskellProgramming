module HP.Ch17 where

import Control.Applicative
--import Data.Map

it2 :: [] (Integer, Integer)
it2 = liftA2 (,) [1, 2] [3, 4]

it2' :: [Int]
it2' =  [(+1), (*2)] <*> [3, 4]

--m = fromList [(2, "hi"), (5, "hello")]
--h = Data.Map.lookup 5 m

f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")]

g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

s = (++) <$> f 3 <*> g 7
n = (+) <$> h 5 <*> m 1
s' = liftA2 (++) (g 9) (f 4)

chapter17 :: IO ()
chapter17 = do
  putStrLn "Chapter 17 - Applicative"
  print it2
  print it2'
  print s
  print n
  print s'

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z


-- Applicative laws
-- 1. Identity
-- prue id <*> v = v
-- 2. Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 3. Homomorphism
-- pure f <*> pure x = pure f x
-- 4 Interchange
-- u <*> pure y = pure ($ y) <*> u

