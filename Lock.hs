module Lock ( Lock
            , newLock
            , aquireLock
            , releaseLock
            ) where

import Control.Concurrent
--
-- Lock
--

type Lock = MVar ()

-- | New Lock
newLock :: IO (Lock)
newLock = newEmptyMVar

-- | Get lock
aquireLock :: Lock -> IO ()
aquireLock v = putMVar v ()

-- | Get lock
releaseLock :: Lock -> IO ()
releaseLock = takeMVar
