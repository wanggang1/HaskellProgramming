module StdListFunctions where

myAnd :: [Bool] -> Bool
myAnd [] = True --termination clause
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False --termination clause
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr (map f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x ys = myAny (==x) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish (map f xs)

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) =
  case f x y of
    GT -> x
    otherwise -> y
  where y = myMaximumBy f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs) =
  case f x y of
    LT -> x
    otherwise -> y
  where y = myMinimumBy f xs

myMaximum :: Ord a => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: Ord a => [a] -> a
myMinimum xs = myMinimumBy compare xs