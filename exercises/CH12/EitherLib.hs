module CH12.EitherLib where

lefts' :: [Either a b] -> [a]
lefts' es = foldr (\e base -> leftToList e ++ base) [] es

leftToList :: Either a b -> [a]
leftToList (Left x) = [x]
leftToList _ = []

rights' :: [Either a b] -> [b]
rights' es = foldr (\e base -> rightToList e ++ base) [] es

rightToList :: Either a b -> [b]
rightToList (Right y) = [y]
rightToList _ = []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' fn (Right x) = Just(fn x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _ (Left x) = fa x
either' _ fb (Right y) = fb y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fb e = either' (\x -> Nothing) (\y -> Just (fb y)) e
