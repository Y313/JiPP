sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr (\x acc -> x >>= (\x1 -> acc >>= (\acc1 -> return (x1:acc1)))) (return []) 

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = foldr (\x acc -> f x >>= (\x1 -> acc >>= (\acc1 -> return (x1:acc1)))) (return [])

forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' l f = mapM' f l
