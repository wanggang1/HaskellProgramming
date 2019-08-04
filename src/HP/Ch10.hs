module HP.Ch10 where

chapter10 :: IO ()
chapter10 = putStrLn "Chapter 10 - Folding lists"

-- associate to the right - traverse spine from left to right
-- the recursion of the spine is conditional based on the strickness of the folding function
-- function applied to the right most element first
-- therefore, the right most elemet evaluated first
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [] = b
myFoldr f b (x:xs) = f x $ myFoldr f b xs

-- associate to the left - traverse spine from left to right
-- at lease 1 recursion, which means the whose spine needs to be evaluated
-- traverse through the whole spine, plus non-strickness evaluating values,
-- affect performence in a negative way, especially over long list.  use foldl', which is strick
-- function applied to the left most element first,
-- therefore, the left most element evaluated first
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f b [] = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs
