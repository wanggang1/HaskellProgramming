module CH17.Apl2 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Fail e
                    | Succ a
                    deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Fail e) = Fail e
  fmap f (Succ a) = Succ $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Succ
  (<*>) (Fail e1) (Fail e2) = Fail $ e1 <> e2
  (<*>) (Fail e) _ = Fail e
  (<*>) _ (Fail e) = Fail e
  (<*>) (Succ f) (Succ x) = Succ $ f x

