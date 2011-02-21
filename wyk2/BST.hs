module BST
( Tree(..)
, insert
, contains
, fromList
, toList
) where
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

instance Functor Tree where
	fmap f (Node val lchild rchild) = Node (f val) (fmap f lchild) (fmap f rchild)
	fmap f Empty = Empty

toList :: Tree a -> [a]
toList Empty = []
toList (Node val lchild rchild) = toList lchild ++ [val] ++ toList rchild


insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a (Node val lchild rchild) =
		if a < val then Node val (insert a lchild) rchild
			   else Node val lchild (insert a rchild)

contains :: (Ord a) => a -> Tree a -> Bool
contains a Empty = False
contains a (Node val lchild rchild) =
		if a == val then True
			    else contains a lchild || contains a rchild

fromList :: (Ord a) => [a] -> Tree a
fromList l = foldl (\acc x -> insert x acc) Empty l
