{-# LANGUAGE MultiParamTypeClasses #-}

module Ch15.MonadHandleIO where

import Ch15.MonadHandle
import qualified System.IO


instance MonadHandle System.IO.Handle IO where
        openFile = System.IO.openFile
        hPutStr = System.IO.hPutStr
        hClose = System.IO.hClose
        hGetContents = System.IO.hGetContents
        hPutStrLn = System.IO.hPutStrLn