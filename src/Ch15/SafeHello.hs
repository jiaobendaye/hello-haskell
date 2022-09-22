module Ch15.SafeHello where

import Ch15.MonadHandle
import Ch15.MonadHandleIO
import System.IO(IOMode(..))

safeHello' :: MonadHandle h m => FilePath -> m ()
safeHello' path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h