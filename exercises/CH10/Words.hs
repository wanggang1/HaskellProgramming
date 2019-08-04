module Words where

stops  = "pbtdkg"
vowels = "aeiou"

verbs = ["am", "is", "are"]
nouns = ["I", "We", "me", "us", "They", "You", "He", "him", "them", "you"]

maybeWords :: [String]
maybeWords = [[x] ++ [v] ++ [y] | x <- stops, x == 'p', v <- vowels, y <- stops]

maybePhrases :: [(String, String, String)]
maybePhrases = [(s, v, o) | s <- nouns, v <- verbs, o <- nouns]

----------- Exercises ----------------
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
{-myAny f xs  = myOr $ map f xs-}
myAny = (myOr .) . map

myElem :: Eq a => a -> [a] -> Bool
{-myElem x xs = myAny (==x) xs-}
myElem x xs = foldr (\y b -> if y == x then True else b) False xs

myReverse :: [a] -> [a]
myReverse = foldl (\b x -> x : b) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x b -> f x : b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x b -> if f x then x : b else b) [] xs

squish :: [[a]] -> [a]
squish = foldr (\xs b -> xs ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
{-squishMap f xs = squish $ map f xs-}
squishMap = (squish .) .  map

squishAgain :: [[a]] -> [a]
squishAgain xss = squishMap (\n -> xss!!n) [0..(length xss - 1)]

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy p xs = ys!!0
  where ys = foldr (\x b -> if null b then [x] else if p x (head b) == GT then [x] else b) [] xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy p xs = ys!!0
  where ys = foldr (\x b -> if null b then [x] else if p x (head b) == LT then [x] else b) [] xs

