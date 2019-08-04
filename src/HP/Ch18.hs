module HP.Ch18 where

import Control.Monad
import Control.Applicative ((*>))

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma

-- same as liftA
liftM :: Monad m
      => (a1 -> r) -> m a1 -> m r
liftM = undefined

-- same as liftA2
liftM2 :: Monad m
       => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 = undefined

-- desugaring the do syntax
sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  yourName <- getLine
  putStrLn yourName

binding' :: IO ()
binding' =
  getLine >>= putStrLn  --flatMap

-- List
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

-- Maybe
data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c = --if Cow's name is Bess, must be under 500
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
      then Nothing
      else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String
                -> Int
                -> Int
                -> Maybe Cow
mkSphericalCow' name' age' weight' =
  noEmpty name' >>=
    \nammy -> noNegative age'  >>=
      \agey -> noNegative weight' >>=
        \weighty -> weightCheck (Cow nammy agey weighty)


---------------------------------------------------
chapter18 :: IO ()
chapter18 = do
  putStrLn "Chapter 18 - Monad"
