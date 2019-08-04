{-# LANGUAGE RankNTypes #-}

-- Natural Transformation:
-- tranform the structure, but leave the value untouched
-- opposite of Functor
module CH16.Nat where

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

runNat :: IO ()
runNat = do
  putStrLn "Natural Transformation..."
