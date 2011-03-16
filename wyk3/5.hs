import Control.Monad.Either
import Control.Monad.Instances

class Functor f => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a->b) -> f a -> f b 

instance Applicative Maybe where
	pure a = return a
	mf <*> ma = do { f <- mf; a <- ma; return (f a) }

instance Applicative (Either e) where
	pure a = return a
	mf <*> ma = do { f <- mf; a <- ma; return (f a) }
