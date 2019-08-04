module HP.Ch01 where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")
