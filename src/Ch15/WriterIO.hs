{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ch15.WriterIO where

import Control.Monad.Writer
import Ch15.MonadHandle
import System.IO(IOMode(..))

data Event = Open FilePath IOMode
                   | Put String String
                   | Close String
                   | GetContents String
                         deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
        deriving (Monad, MonadWriter [Event])

instance Functor WriterIO where
    fmap = liftM

instance Applicative WriterIO where
    pure = return
    (<*>) = ap


runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return ""