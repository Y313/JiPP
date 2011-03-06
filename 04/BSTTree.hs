module BSTTree where

data Tree a = None | Node (Tree a) a (Tree a) deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert a None = Node None a None
insert a (Node l w r)
	| a < w = Node (insert a l) w r
	| otherwise = Node l w (insert a r)

toList :: Tree a -> [a]
toList t = toList t [] where
	toList None a = a
	toList (Node l w r) a = toList l (w:(toList r a))

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert None

contains :: Eq a => a -> Tree a -> Bool
contains a t = elem a (toList t)
