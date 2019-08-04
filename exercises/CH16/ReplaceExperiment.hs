module CH16.ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]] -- 3 structures: [], Maybe, []
lms = [Just "Ave", Nothing, Just "woohoo"]

--  specific to lms
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- lift replaceWithP over functor
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- specific to lms, value inside structure [] is replaced
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- lift it twice
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- specific to lms, value inside nested structures [Maybe _] is replaced
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- lift it 3 times
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- specific to lms, value inside nested structures [Maybe [_]] is replaced
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

replace :: IO ()
replace = do
  putStr "replaceWithP' lms:  "
  print (replaceWithP' lms)
  putStr "liftedReplace lms:  "
  print (liftedReplace lms)
  putStr "liftedReplace' lms:  "
  print (liftedReplace' lms)
  putStr "twiceLifted lms:  "
  print (twiceLifted lms)
  putStr "twiceLifted' lms:  "
  print (twiceLifted' lms)
  putStr "thriceLifted lms:  "
  print (thriceLifted lms)
  putStr "thriceLifted' lms:  "
  print (thriceLifted' lms)

