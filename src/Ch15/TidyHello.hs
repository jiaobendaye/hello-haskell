module Ch15.TidyHello where

import Ch15.MonadHandle
import Ch15.SafeHello
import Ch15.MonadHandleIO
import Control.Monad.Trans (MonadIO(..))
import System.Directory

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
  safeHello' path
  liftIO (removeFile path)

tidyHello' :: (MonadIO m, MonadHandle h m) => FilePath -> m ()
tidyHello' path = do
  safeHello' path
  liftIO (removeFile path)