{-# LANGUAGE ViewPatterns #-}

module CH17.Exercises where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity $ f a


newtype Constant a b = Constant { getConstant :: a }
                       deriving (Eq, Ord, Show)

-- Fix tyep a in Constant (the 1st type param because curry), a becomes the structure
-- For fmap, f is applied to the value in the structure, in the case of Constant, the value is ignored
instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x
-- For Applicative, the structure must be monoidal (can be combined).  In case of Constant, the value
-- can be ignored, so the <*> is expressed in point-free style.
instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x `mappend` y)


data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap _ Nothing'  = Nothing'
  fmap f (Just' x) = Just' $ f x

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*>  _ = Nothing'
  _ <*>  Nothing' = Nothing'
  Just' f  <*> Just' x = Just' $ f x

validateLength :: Int -> String -> Maybe' String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing'
  else Just' s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address deriving (Eq, Show)

mkName :: String -> Maybe' Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe' Address
mkAddress a = fmap Address $ validateLength 100 a

mkPerson :: String -> String -> Maybe' Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

