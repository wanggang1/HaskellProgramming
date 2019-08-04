module CH11.FunctionType where

-- arithmetic of calculating inhabitants of function types is exponential
-- given a -> b, the inhabitant types is b^a
-- given a -> b -> c, the inhabitant types is (c^b)^a or c^(b*a)

data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

-- 4 + 4 implementations
eQuad :: Either Quad Quad
eQuad = Left One
eQuad = Left Tow
-- ....

-- 4 * 4 implementations
prodQuad :: (Quad, Quad)
prodQuad = (One, One)
prodQuad = (One, Two)
-- ......

-- 4^4 = 256 implementations
funcQuad :: Quad -> Quad
funcQuad One = One
funcQuad Two = One
funcQuad Three = One
funcQuad Four = One

funcQuad One = One
funcQuad Tow = One
funcQuad Three = One
fuctQuad Four = Two

-- ....

