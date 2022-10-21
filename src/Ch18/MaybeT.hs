{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Ch18.MaybeT where

import Control.Monad
import Control.Monad.State

-- https://blog.csdn.net/zwvista/article/details/78167779

newtype MaybeT m a = MaybeT { 
  runMaybeT :: m (Maybe a) 
  } 

-- ‘fail’ is not a (visible) method of class ‘Monad
instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  -- fail = failMT

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = lift . return
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
    unwrapped <- runMaybeT x
    case unwrapped of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)
  
altBindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `altBindMT` f =
    MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  
returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing


instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)