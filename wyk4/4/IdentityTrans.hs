module IdentityTrans where
import Control.Monad(MonadPlus(mzero, mplus))
import Control.Monad.Trans

newtype IdentityT m a = IdentityT { runIdentityT:: m a }

instance (Functor m) => Functor (IdentityT m) where
	fmap f = IdentityT . (fmap f) . runIdentityT

instance (Monad m) => Monad (IdentityT m) where
	return = IdentityT . return
	a >>= b = IdentityT $ runIdentityT a >>= \x -> runIdentityT (b x)

instance MonadTrans IdentityT where
	lift = IdentityT

instance MonadPlus m => MonadPlus (IdentityT m) where
	mzero = IdentityT mzero
	a `mplus` b = IdentityT $ runIdentityT a `mplus` runIdentityT b
