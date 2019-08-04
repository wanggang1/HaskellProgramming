module HP.Ch03 where

import Data.Char
import Data.List


myGreeting :: String
myGreeting = (++) "hello" " chapter 03!"

chapter03 :: IO ()
chapter03 = do
    putStrLn myGreeting
    putStrLn secondGreeting where
    secondGreeting =
        (++) "hello" ((++) " " "world")


------------------ Exercise 2 ------------------
append :: String -> String -> String
append x y = x ++ y

after :: Int -> String -> String
after i x = take 1 (drop i x)

lastWord :: String -> String
lastWord = laste . convertToWords

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex i = "Curry is awesome!" !! i


convertToWords   :: String -> [String]
convertToWords s =  case dropWhile isSpace s of
                        "" -> []
                        s' -> w : convertToWords s''
                            where (w, s'') = break isSpace s'

laste :: [a] -> a
laste []     = error "empty lists"
laste [x]    = x
laste (x:xs) = laste xs


------------------ Reverse Exercise ------------------
rvrs :: String -> String
rvrs x = intercalate " " (flipped (convertToWords x))


flipped :: [String] -> [String]
flipped []     = []
flipped [x]    = [x]
flipped (x : xs) = (flipped xs) ++ [x]

reversePhrase :: IO ()
reversePhrase = putStrLn (rvrs "Curry is awesome")

