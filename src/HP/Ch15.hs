module HP.Ch15 where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

chapter15 :: IO ()
chapter15 = putStrLn "Chapter 15 - Monoid and Semigroup"

---------------- Monoid ---------------------------
data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a1) (Only a2) = Only $ mappend a1 a2
  mappend _ _ = Nada

{-
An orphan instance is when an instance is defined for a datatype and typeclass,
but not in the same module as either the declaration of the typeclass or the
datatype.  If you don’t “own” the typeclass or the datatype, newtype it!
-}
newtype Listy a = Listy [a] deriving (Eq, Show)

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy l) (Listy l') = Listy $ mappend l l'

-------- test Monoid laws with QuickCheck ----------------
monoidAssoc:: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

-- wrong implementation of Monoid, doesn't obey left and right identity laws
instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

---------------- Semigroup -------------------------
-- has a binary operation and associativity, but no identity
-- super class of Monoid: class Semigroup a => Monoid a where
-- NonEmpty is an example, it has no identity because emppy list is not allowed

newtype NonEmpty a = NonEmpty (a, [a])
                     deriving (Eq, Ord, Show)


testMonoid :: IO ()
testMonoid = do
  quickCheck (monoidAssoc :: String -> String -> String -> Bool)
  quickCheck (monoidLeftIdentity :: String -> Bool)
  quickCheck (monoidRightIdentity :: String -> Bool)
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

