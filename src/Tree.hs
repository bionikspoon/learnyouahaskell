data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving ( Read, Eq)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) | x == a = Node x left right
                                 | x < a  = Node a (treeInsert x left) right
                                 | x > a  = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right) | x == a = True
                               | x < a  = treeElem x left
                               | x > a  = treeElem x right

nums = [8, 6, 4, 1, 7, 3, 5]
numsTree = from nums

shortTree = from [1, 2, 3]

from :: (Ord a) => [a] -> Tree a
from = foldr treeInsert EmptyTree

instance Functor Tree where
  fmap f EmptyTree           = EmptyTree
  -- rebalance tree?
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

    -- treeInsert (fmap f right) $ treeInsert (fmap f left) $ singleton (f a)


instance (Show a) => Show (Tree a) where
  show EmptyTree                    = "_"
  show (Node a EmptyTree EmptyTree) = show a
  show (Node a left right) =
    show a ++ " (" ++ show left ++ "<>" ++ show right ++ ") "

