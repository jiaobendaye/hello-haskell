{-# LANGUAGE FunctionalDependencies #-}

module Ch15.MonadHandleIO where
import Ch15.MonadHandle
import Control.Monad.Trans (MonadIO(..))
import qualified System.IO


instance MonadHandle System.IO.Handle IO where
        openFile = System.IO.openFile
        hPutStr = System.IO.hPutStr
        hClose = System.IO.hClose
        hGetContents = System.IO.hGetContents
        hPutStrLn = System.IO.hPutStrLn

class (MonadHandle h m, MonadIO m) => MonadHandleIO h m | m -> h

instance MonadHandleIO System.IO.Handle IO
