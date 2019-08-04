module TypeKwonDo where

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = (g . f) i

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xTOy yTOwz x = fst ((yTOwz . xTOy) x)

id1 :: a -> a -> a
id1 x y = x

id2 :: Num a => a -> a -> a
id2 x y = x + y
