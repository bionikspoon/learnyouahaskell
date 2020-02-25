data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Read, Eq)

instance Functor Tree where
  fmap f EmptyTree           = EmptyTree
  -- rebalance tree?
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

instance (Show a) => Show (Tree a) where
  show = showTree

instance Foldable Tree where
  foldMap fn EmptyTree = mempty
  foldMap fn (Node a left right) =
    foldMap fn left `mappend` fn a `mappend` foldMap fn right

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

-- >>> numsTree
--    ├──8
--    │
-- ├──7
-- │  │
-- │  ├──6
-- │
-- 5
-- │
-- │  ├──4
-- │  │
-- ├──3
--    │
--    ├──1
-- <BLANKLINE>
--

shortTree = from [1, 2, 3]

-- >>> shortTree
-- ├──╢
-- │
-- 3
-- │
-- │  ├──╢
-- │  │
-- ├──2
--    │
--    ├──1
-- <BLANKLINE>
--

from :: (Ord a) => [a] -> Tree a
from = foldr treeInsert EmptyTree

showTree :: Show a => Tree a -> String
showTree = showTreeWith False True

showTreeWith :: Show a => Bool -> Bool -> Tree a -> String
showTreeWith hang wide t | hang      = showsTreeHang wide [] t ""
                         | otherwise = showsTree wide [] [] t ""

showsTree :: Show a => Bool -> [String] -> [String] -> Tree a -> ShowS
showsTree wide lbars rbars t = case t of
  EmptyTree                  -> showsBars lbars . showString "╢\n"
  Node x EmptyTree EmptyTree -> showsBars lbars . shows x . showString "\n"
  Node x l r ->
    showsTree wide (withBar rbars) (withEmpty rbars) r
      . showWide wide rbars
      . showsBars lbars
      . shows x
      . showString "\n"
      . showWide wide lbars
      . showsTree wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: Show a => Bool -> [String] -> Tree a -> ShowS
showsTreeHang wide bars t = case t of
  EmptyTree                  -> showsBars bars . showString "╢\n"
  Node x EmptyTree EmptyTree -> showsBars bars . shows x . showString "\n"
  Node x l r ->
    showsBars bars
      . shows x
      . showString "\n"
      . showWide wide bars
      . showsTreeHang wide (withBar bars) l
      . showWide wide bars
      . showsTreeHang wide (withEmpty bars) r

showsBars :: [String] -> ShowS
showsBars bars = case bars of
  [] -> id
  _  -> showString (concat (reverse (tail bars))) . showString node

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "│\n"
  | otherwise = id

node :: String
node = "├──"

withBar :: [String] -> [String]
withBar bars = "│  " : bars

withEmpty :: [String] -> [String]
withEmpty bars = "   " : bars

