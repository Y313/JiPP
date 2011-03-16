import Control.Monad.Reader
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumber :: Tree a -> Tree Int
renumber tree = renumberFrom 0 tree
		where
		renumberFrom n Empty = Empty
		renumberFrom n (Node a l r) =
				Node n (renumberFrom (n + 1) l) (renumberFrom (n + 1) r)

renumber' :: Tree a -> Tree Int
renumber' tree = runReader (renumberM tree) 0
		 where
		 renumberM Empty = return Empty
		 renumberM (Node a l r) = do
			depth <- ask
			l <- local (1+) $ renumberM l
			r <- local (1+) $ renumberM r
			return $ Node depth l r
