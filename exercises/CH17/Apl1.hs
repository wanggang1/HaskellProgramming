module CH17.Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

-- defined in Test.QuickCheck.Arbitrary
{-
instance Arbitrary a  => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary
-}


data List a = Nil | Cons a (List a)
               deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x)  (fmap f xs)


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

instance Eq a => EqProp (List a) where
  (=-=) = eq

arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList m
    | m == 0 = return Nil
    | m > 10 = arbitraryList 10  -- capped at 10 to avoid long List
    | otherwise = Cons <$> arbitrary <*> (arbitraryList (m-1))

instance Arbitrary a  => Arbitrary (List a) where
  arbitrary = sized arbitraryList

---------- test laws with checkers ------------
zl = ZipList [1 :: Sum Int]
l = Cons (1 :: Int)  Nil

testApl1 :: IO()
testApl1 = do
  quickBatch $ monoid zl
  --quickBatch $ applicative l


