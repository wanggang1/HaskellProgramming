module EqTypeclass where

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn i == TisAn i' = i == i'

-------------------------------------
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i j) (Two i' j') = i == i' && j == j'

-------------------------------------
data StringOrInt =
  TisAString String | TisAnInt Int

instance Eq StringOrInt where
  (==) (TisAString s) (TisAString s') = s == s'
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) _ _ = False

-------------------------------------
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

-------------------------------------
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-------------------------------------
data Which a =
  ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _  = False

-------------------------------------
data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
  (==) _ _ = False

