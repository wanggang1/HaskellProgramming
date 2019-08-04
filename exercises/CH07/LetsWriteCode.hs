tensDigit :: Integral a => a -> a
tensDigit x = d
  where tens = x `div` 10
        d    = tens `mod` 10

tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
  where (tens, ones) = x `divMod` 10
        d            = tens `mod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where d  = x `div` 100
        d2 = d `mod` 10

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y t =
  case t of
    True  -> x
    False -> y

foldBool :: a -> a -> Bool -> a
foldBool x y t
  | t         = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
