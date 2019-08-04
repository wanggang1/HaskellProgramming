module HP.Ch04 where

------Exercise: Mood Swing ---------
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
     then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhhh."
  where cool = coolness == "gang"

isPalindrome :: Eq a => [a]  -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs i = if i < 0
             then -i
          else i

flipCross :: (a, b) -> (c, d) -> ((b, d), (a, c))
flipCross t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

