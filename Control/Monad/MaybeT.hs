module Control.Monad.MaybeT (MaybeT(..)) where
import Data.Maybe
import Data.Functor
import Control.Monad
import Control.Applicative
import Control.Monad.Trans


data (Monad m) => MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m) => Monad (MaybeT m) where
    MaybeT m >>= f = MaybeT $ m >>= maybe (return Nothing) (runMaybeT . f)
    return a = MaybeT $ return $ Just a

instance (Monad m) => Functor (MaybeT m) where
    fmap f (MaybeT m) = MaybeT $ m >>= return . maybe Nothing (Just . f)

instance (Monad m) => Applicative (MaybeT m) where
    pure f = MaybeT $ return (Just f)
    m <*> o = m >>= \f -> o >>= \obj -> return $ f obj
