-- functions in lambda form

mTh :: Num a => a -> a -> a -> a
  {- function form: mTh x y z = x * y * z -}
{-mTh x y = \z -> x * y * z-}
{-mTh x = \y -> \z -> x *  y * z-}
mTh = \x -> \y -> \z -> x * y * z

addOne :: Num a => a -> a
  {- function form: addOne x = x + 1-}
addOne = \x -> x + 1

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
                  True -> f n
                  False -> n
                  where f = \x -> x + 1

addFive :: (Num a, Ord a) => a -> a -> a
{-addFive = \x -> \y -> (if x > y then y else x) + 5-}
addFive = \x y -> (if x > y then y else x) + 5

mshow :: String -> String -> IO()
mshow x y = putStrLn(x ++ " " ++ y)

{-mflip f = \x -> \y -> f y x-}
mflip f x y = f y x
