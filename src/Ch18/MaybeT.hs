module Ch18.MaybeT where

import Control.Monad


newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a) }

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


-- TODO ‘fail’ is not a (visible) method of class ‘Monad
-- instance (Monad m) => Monad (MaybeT m) where
--   return = returnMT
--   (>>=) = bindMT
--   fail = failMT

-- instance MonadTrans MaybeT where
--     lift m = MaybeT (Just `liftM` m)