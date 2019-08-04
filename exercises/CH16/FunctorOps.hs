{-# LANGUAGE ViewPatterns #-}

module CH16.FunctorOps where

import Test.QuickCheck
--import Test.QuickCheck.Function
import CH16.FunctorChecks

newtype Identity a = Identity a deriving (Eq, Show)

data Pair a = Pair a a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

----------------------------------------------------------
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a1 <- arbitrary
  a2 <- arbitrary
  return (Pair a1 a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a1 <- arbitrary
  b1 <- arbitrary
  return (Two a1 b1)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

----------------------------------------------------------
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Functor (Two a) where
  fmap f (Two a1 b1) = Two a1 (f b1)

----------------------------------------------------------
type IntIdentity =  Identity Int -> Bool
type StringIdentity = Identity String -> Bool
type IntIdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool
type IntPair = Pair Int -> Bool
type StringPair = Pair String -> Bool
type IntPairFC = Pair Int -> IntToInt -> IntToInt -> Bool
type StringIntTwo = Two String Int -> Bool
type StringIntTwoFC = Two String Int -> IntToInt -> IntToInt -> Bool

testFunctor :: IO()
testFunctor = do
  quickCheck (functorIdentity :: IntIdentity)
  quickCheck (functorIdentity :: StringIdentity)
  quickCheck (functorCompose' :: IntIdentityFC)
  quickCheck (functorIdentity :: IntPair)
  quickCheck (functorIdentity :: StringPair)
  quickCheck (functorCompose' :: IntPairFC)
  quickCheck (functorIdentity :: StringIntTwo)
  quickCheck (functorCompose' :: StringIntTwoFC)

