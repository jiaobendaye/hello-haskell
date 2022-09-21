module Ch14.State (
  State
  ,getSt
  ,putSt
  ,runState
)where

import Control.Monad (liftM, ap)

newtype State s a = State {
        runState :: s -> (a, s)
}


returnState :: a -> State s a 
returnState a = State (\s-> (a, s)) 

bindState :: State s a -> (a -> State s b) -> State s b 
bindState m k = State (\s -> let (a, s') = runState m s 
                                in runState ( k a ) s') 

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
        return = returnState
        (>>=) = bindState


getSt :: State s s 
getSt = State $ \s -> (s, s)

putSt :: s ->  State s ()
putSt s = State $ const ((), s)
