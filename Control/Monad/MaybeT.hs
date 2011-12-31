module Control.Monad.MaybeT (MaybeT(..)) where
import Data.Maybe
import Data.Functor
import Control.Monad
import Control.Monad.Trans


data (Monad m) => MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m) => Monad (MaybeT m) where
    MaybeT m >>= f = MaybeT $ do
                   r <- m
                   case r of 
                     Nothing -> return Nothing
                     Just x -> runMaybeT $ f x
    return a = MaybeT $ return $ Just a

instance (Monad m) => Functor (MaybeT m) where
    fmap f (MaybeT m) = MaybeT $ do
                      r <- m
                      case r of
                        Nothing -> return Nothing
                        Just x -> return $ Just (f x)
