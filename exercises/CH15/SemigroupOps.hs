module CH15.SemigroupOps where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

data Or a b = Fst a | Snd b deriving (Eq, Show)

newtype Combine a b = Combine { unCombine :: (a -> b) }

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

-------------------------------------------------------------
instance Semigroup Trivial where
  _ <> _ = Trivial

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance Semigroup BoolConj where
  (<>) (BoolConj b) (BoolConj b') = BoolConj (b && b')

instance Semigroup BoolDisj where
  (<>) (BoolDisj b) (BoolDisj b') = BoolDisj (b || b')

instance Semigroup (Or a b) where
  (<>) (Fst _) (Fst a2) = Fst a2
  (<>) (Fst _) (Snd b) = Snd b
  (<>) (Snd b) _ = Snd b

-- Orphan instance: instance Semigroup Bool
instance Semigroup Bool where
  (<>) b b' = b && b'

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) =
    Combine $ \a -> (f a) <> (g a)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure' a1) (Failure' a2) = Failure' (a1 <> a2)
  (<>) (Failure' a) (Success' _) = Failure' a
  (<>) (Success' _) (Failure' a) = Failure' a
  (<>) (Success' b) (Success' b') = undefined

-------------------------------------------------------------
instance Arbitrary Trivial where
  arbitrary = return Trivial

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = orGen

--------------------------------------------------------------
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool
type TwoStringTrivialAssoc = Two String Trivial -> Two String Trivial -> Two String Trivial -> Bool
type OrStringBoolAssoc = Or String Bool -> Or String Bool -> Or String Bool -> Bool

testSemigroup :: IO()
testSemigroup = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityStringAssoc)
  quickCheck (semigroupAssoc :: TwoStringTrivialAssoc)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrStringBoolAssoc)
{-
testCombine :: IO()
testCombine = do
  let f = Combine $ \n -> Sum (n + 1)
  let g = Combine $ \n -> Sum (n - 1)
  let r1 = unCombine (f <> g) $ 0
  putStrLn (show r1)
  let r2 = unCombine (f <> g) $ 1
  putStrLn (show r2)
  let r3 = unCombine (f <> f) $ 1
  putStrLn (show r3)
  let r4 = unCombine (g <> f) $ 1
  putStrLn (show r4)
-}
