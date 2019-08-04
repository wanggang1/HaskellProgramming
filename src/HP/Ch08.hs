module HP.Ch08 where

chapter08 :: IO ()
chapter08 = putStrLn "Chapter 08 - Recursion"

-- recursion with function composition:
-- self referential, the result of function application is
-- fed into the next application of itself
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times n = applyTimes times (+1) n

-- Fibonacci number, not tail recursion
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- Not tail recursion
sumAll :: (Eq a, Num a) => a -> a
sumAll 1 = 1
sumAll x = x + sumAll (x - 1)

multBy :: Integral a => a -> a -> a
multBy number times = go number times 0
  where go n t sum
         | t == 0 = sum
         | otherwise = go n (t - 1) (sum + n)

