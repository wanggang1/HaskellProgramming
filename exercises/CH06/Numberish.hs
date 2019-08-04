class Numberish a where
  fromNumber :: Int -> a
  toNumber :: a -> Int

data Age  =
    Age Int
    deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

-- how, fromNumber and toNumber are in scope ???
sumN :: Numberish a => a -> a -> a
sumN a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime