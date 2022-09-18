module Ch15.Supply(
  Supply
  , next
  , runSupply
) where

import Ch14.State
import Control.Monad (liftM, ap)

newtype Supply s a = S (State [s] a)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Functor (Supply s) where
    fmap = liftM

instance Applicative (Supply s) where
    pure = S . pure
    (<*>) = ap

instance Monad (Supply s) where
    s >>= m = S (unwrapS s >>= unwrapS . m)

next :: Supply a (Maybe a)
next = S $ do st <- getSt
              case st of
                [] -> return Nothing
                (x:xs) -> do putSt xs
                             return (Just x)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs