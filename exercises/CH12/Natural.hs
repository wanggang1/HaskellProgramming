module CH12.Natural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (validNat i)

validNat :: Integer -> Nat
validNat 0 = Zero
validNat i = Succ (validNat $ i-1)
