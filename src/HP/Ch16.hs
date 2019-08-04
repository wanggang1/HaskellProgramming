module HP.Ch16 where

chapter16 :: IO ()
chapter16 = putStrLn "Chapter 16 - Functor"

data WhoCares a = ItDoesnt
                | Matter a
                | WhatThisIsCalled
                deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)


---------------------------------------------
-- Functor is higher kind type: * -> *
-- for types with more than 1 type parameters
-- i.e., Either a b: * -> * -> *
-- the Functor instance will have 1 type fixed as type constant

data Two a b = Two a b
               deriving (Eq, Show)

-- type a is part of the structure, untouchable
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Or a b = First a
            | Second b
            deriving (Eq, Show)

-- type a is part of the structure, untouchable
instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)


data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)


data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- IO is functor
-- fmap (++ " and me too!") getLine
meToo :: IO String
meToo = do
  input <- getLine
  return (input ++ " and me too!")

