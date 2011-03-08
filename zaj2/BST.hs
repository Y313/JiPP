module BST
( Tree(..)
, insert
, contains
, fromList
, toList
) where
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

{- a) instancja klasy Functor -}
instance Functor Tree where
    fmap f (Empty) = Empty 
    fmap f (Node a lson rson) = Node (f a) (fmap f lson) (fmap f rson)

{- b) funkcja toList drzewo do listy porzadek infiksowy -}
toList :: Tree a -> [a]
toList Empty = []
toList (Node a lson rson) = toList(lson) ++ [a] ++ toList(rson)

{- c) drzewa BST - funkcje insert, contains, fromList -}
insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a (Node val lson rson) = 
    if a < val then Node val (insert a lson) rson
    else Node val lson (insert a rson)

contains :: (Ord a) => a -> Tree a -> Bool
contains a Empty = False
contains a (Node val lson rson) =
	if a == val then True
	else (contains a lson) || (contains a rson)

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList (h:xs) = insert h (fromList xs)
