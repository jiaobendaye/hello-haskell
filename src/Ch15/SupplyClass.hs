{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

module Ch15.SupplyClass
        (
          MonadSupply(..)
        , S.Supply
        , S.runSupply
        ) where

import qualified Ch15.Supply as S

class (Monad m) => MonadSupply s m | m -> s where
        next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
        next = S.next

showTwoClass :: (Show s, Monad m, MonadSupply s m) => m String
showTwoClass = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)