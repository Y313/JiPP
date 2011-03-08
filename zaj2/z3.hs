import BST

instance Functor (Either e) where
  -- fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Right a) = Right (f a)
  fmap f (Left a) = Left a

reverseRight :: Either e [a] -> Either e [a]
reverseRight (Left a) = (Left a)
reverseRight (Right a) = Right $ reverse a

reverseRight' :: Either e [a] -> Either e [a]
reverseRight' a = fmap reverse a

class Functor f => Pointed f where
  pure :: a -> f a
  
instance Pointed [] where
  pure a = [a]

instance Pointed Maybe where
  pure a = Just a
  
instance Pointed Tree where
  pure a = Node a Empty Empty
