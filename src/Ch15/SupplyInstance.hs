{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses #-}

module Ch15.SupplyInstance() where

import Control.Monad
import Ch15.SupplyClass

newtype Reader e a = R { runReader :: e -> a }

instance Functor (Reader a) where
    fmap = liftM

instance Applicative (Reader a) where
    pure a = R $ \_ -> a
    (<*>) = ap

instance Monad (Reader e) where
    m >>= k = R $ \r -> runReader (k (runReader m r)) r
  
ask :: Reader e e
ask = R id

newtype MySupply e a = MySupply { runMySupply :: Reader e a }
    deriving (Monad)

instance Functor (MySupply a) where
    fmap = liftM

instance Applicative (MySupply a) where
    pure = return
    (<*>) = ap

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
             v <- ask
             return (Just v)

    -- more concise:
    -- next = MySupply (Just `liftM` ask)
