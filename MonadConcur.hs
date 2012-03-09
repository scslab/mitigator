-- | Generalize MVar to arbitrary monad.
module MonadConcur ( MVar
                   , MonadConcur(..)
                   ) where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as M

-- | Monad with MVar primitives.
class Monad m => MonadConcur m where
  newEmptyMVar :: m (MVar a)
  newMVar      :: a -> m (MVar a)
  takeMVar     :: MVar a -> m a
  putMVar      :: MVar a -> a -> m ()
  withMVar     :: MVar a -> (a -> m b) -> m b
  fork         :: m () -> m ()

instance MonadConcur IO where
  newEmptyMVar = M.newEmptyMVar
  newMVar      = M.newMVar
  takeMVar     = M.takeMVar
  putMVar      = M.putMVar
  withMVar     = M.withMVar
  fork io      = void $ forkIO io
