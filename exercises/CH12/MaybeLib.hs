module CH12.MaybeLib where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ fn (Just y) = fn y

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- for Just vaules only
catMaybes :: [Maybe a] -> [a]
catMaybes ms = concat $ map maybeToList ms

-- sequence
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = case (length $ filter isNothing ms) > 0 of
  True -> Nothing
  False -> Just (catMaybes ms)
