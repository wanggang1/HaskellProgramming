module CH11.SumProductTypes where

import Data.Int

-- cardinality is 4
data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- cardinality is 258
data NumberOrBool =
    Numba Int8
  | Booly Bool
  deriving (Eq, Show)


--product type
--terribly large cardinality
data Person = MkPerson String Int deriving (Eq, Show)

namae :: Person -> String
namae (MkPerson s _) = s

--Record syntex
data Person1 = Person1 {name :: String, age :: Int}
               deriving (Eq, Show)


-- distributive property
-- a * (b + c) = a * b + a * c
-- product type distributes over sum types
-- sum of product is normal form (can't do further rewrite)
data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show
data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType) --product of sum is not normal form

-- rewrite Author in normal form
-- Fiction and NonFiction is in the type definition, not hidden in sum type BookType
-- no further evaluation can be done to these data contructors
data Author1 = Fiction1 AuthorName
             | NonFiction1 AuthorName
             deriving (Eq, Show)

