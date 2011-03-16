import Control.Monad.State
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumberTree :: Tree a -> Tree Int
renumberTree tree = fst $ runState (renumberTree' tree) 0
	where
	renumberTree' Empty = return Empty
	renumberTree' (Node a l r) = do
		l' <- renumberTree' l
		mid <- get
		modify (1+)
		r' <- renumberTree' r
		return (Node mid l' r')

renumberTree2 tree = fst $ renumberTree2' tree 0
		where
		renumberTree2' Empty n = (Empty, n)
		renumberTree2' (Node a l r) n =
			let (l', n') = renumberTree2' l n in
			let (r', n'') = renumberTree2' r (n' + 1) in
			(Node (n') l' r', n'')

toList Empty = []
toList (Node a l r) = toList l ++ [a] ++ toList r
