module HP.Ch07 where

chapter07 :: IO ()
chapter07 = putStrLn "Chapter 07 - More Functional Patterns"

-- Pattern matching
-- data structure: newtype is a special case of data, only allow 1 constructor and 1 field

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser |
            RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber account))
          = putStrLn $ name ++ " " ++ show account

-- sum type ( a union of values, ||)
data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

-- product type ( a intersect of types, &&, composite type?)
data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica           _ = False

whereTheyLive :: Penguin -> WherePenguinsLive
whereTheyLive (Peng wpl) = wpl

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin                _ = False

-- pattern match tuples
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

add2up :: Num a => (a, a) -> a
add2up (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

thrd3 :: (a, b, c) -> c
thrd3 (_, _, x) = x

-- case expression using pattern matching
pal :: Eq a => [a] -> String
pal xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

funcC :: Ord a => a -> a -> a
funcC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 x =
  case even x of
    True -> x + 2
    False -> x

nums :: (Ord a, Num a) => a -> Int
nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

-- higher order function
myFlip :: (a -> b -> c ) -> b -> a -> c
{-myFlip f x y = f y x-}
myFlip f = \x y -> f y x

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee
             -> Employee
             -> IO()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (myFlip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

-- guard block (same as if-then-else)
myAbs :: (Num a, Ord a) => a -> a
myAbs x
  | x < 0 = (-x)
  | otherwise = x

--function composition
take5From :: Integer -> [Integer]
take5From i = take 5 . enumFrom $ i

-- pointfree style - function composition without specify arguments
take6From :: Integer -> [Integer]
take6From = take 6 . enumFrom  -- \i -> take 6 (enumFrom i)

