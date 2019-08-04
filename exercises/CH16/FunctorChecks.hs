{-# LANGUAGE ViewPatterns #-}

module CH16.FunctorChecks where

import Test.QuickCheck
import Test.QuickCheck.Function

---------------------------------------------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == fmap (g . f) x

-- let QuickCheck to generate functions
functorCompose' :: (Eq (f c), Functor f) =>
                   f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
----------------------------------------------------------

add1 :: Int -> Int
add1 x = x + 1

showNumb :: Int -> String
showNumb x = show x

comp :: [Int] -> Bool
comp = functorCompose add1 showNumb

li :: [Int] -> Bool
li x = comp (x :: [Int])

type IntToInt = Fun Int Int
type IntToString = Fun Int String
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
type IntStrFC = [Int] -> IntToInt -> IntToString -> Bool

checkFunctor :: IO()
checkFunctor = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorIdentity :: [Char] -> Bool)
  quickCheck li
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorCompose' :: IntStrFC)

