{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- By defualt, it's MonomorphismRestriction, so example :: Integer
-- By adding the header NoMonomorphismRestriction, example :: Num t => t
example = 1
