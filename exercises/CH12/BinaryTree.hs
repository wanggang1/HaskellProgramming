module CH12.BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

