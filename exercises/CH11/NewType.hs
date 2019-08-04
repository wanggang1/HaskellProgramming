{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module CH11.NewType where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- reuse type class instance of the type that newtype contains
instance TooMany Cows where
  tooMany (Cows n) = tooMany n

-- use pragma GeneralizedNewtypeDeriving defined at the top of the file
-- no need to define instance of TooMany for Horses
newtype Horses = Horses Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, s) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n1, n2) = tooMany (n1 + n2)
