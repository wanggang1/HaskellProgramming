module HP.Ch02 where

mult1      = x * y
  where  x = 5
         y = 6

mult :: Int -> Int -> Int
mult x y = x * y

divide :: Double -> Double -> Double
divide x y = x / y

infixl 5 @@
(@@) = divide
