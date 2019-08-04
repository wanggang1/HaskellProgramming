module HP.Ch06 where

chapter06 :: IO ()
chapter06 = putStrLn "Chapter 06 - Typeclasses"

--------------------------------------------------------
data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

---------------------------------------------------------
data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

---------------------------------------------------------
data Date =
  Date DayOfWeek Int
  deriving Show

instance Eq Date where
  (==) (Date weekDay dayOfMonth) (Date weekDay' dayOfMonth') =
    weekDay == weekDay' && dayOfMonth == dayOfMonth'
