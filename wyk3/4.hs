sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
		l <- x
		ls <- sequence' xs
		return (l:ls)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do
		l <- (f x)
		ls <- mapM' f xs
		return (l:ls)

forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' l f = mapM' f l
