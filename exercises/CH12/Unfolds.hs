module CH12.Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate fn x = x : myIterate fn (fn x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr fn x = case (fn x) of
  Nothing -> []
  Just (x', y) -> x' : (myUnfoldr fn y)

myIterate' :: (a -> a) -> a -> [a]
myIterate' fn x = myUnfoldr (\b -> Just (b, fn b)) x
