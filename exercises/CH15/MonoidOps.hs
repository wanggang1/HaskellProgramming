module CH15.MonoidOps where

import Data.Monoid
import Test.QuickCheck
import CH15.SemigroupOps

instance Monoid Trivial where
  mempty = Trivial
  mappend _ _ = Trivial

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity $ x `mappend` y

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj t) (BoolConj t') = BoolConj $ t && t'

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj t) (BoolDisj t') = BoolDisj $ t || t'


-------- test Monoid laws with QuickCheck ----------------
monoidAssoc:: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


testMonoidOps :: IO ()
testMonoidOps = do
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentityStringAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)


