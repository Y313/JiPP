module MaybeTrans(MaybeT(..)) where
import Control.Monad.Trans
import Control.Monad(MonadPlus(mzero, mplus), liftM)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
	lift = MaybeT . liftM Just

instance (Monad m) => Monad (MaybeT m) where
	fail _ = MaybeT (return Nothing)
	return = lift . return
	x >>= f = MaybeT $ do
		v <- runMaybeT x
		case v of
			Nothing -> return Nothing
			Just y -> runMaybeT $ f y

instance (Monad m) => MonadPlus (MaybeT m) where
	mzero = MaybeT (return Nothing)
	mplus x y = MaybeT $ do
		v <- runMaybeT x
		case v of
			Nothing -> runMaybeT y
			Just _ -> return v
