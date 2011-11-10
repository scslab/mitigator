{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | General interface to a time-mitigated IO monad.

module Mitigator.Storage ( MIOStorage, StorageMitigated
                         , quant
                         ) where

import System.Posix.Unistd
import System.IO
import System.Posix.Clock
import Lock
import Mitigator
import Control.Monad.State.Strict hiding (get, put)
import qualified Data.ByteString.Char8 as BS

-- | Time stamp.
newtype BuffBytes = BuffBytes { unBuffBytes :: BS.ByteString }
  deriving (Eq, Show)

-- | Time stamp difference in picoseconds.
newtype BuffSizeDiff = BuffSizeDiff { unBuffSizeDiff :: Integer }
  deriving (Eq, Ord, Num, Show)

-- | Type of mitigated IO.
type MIOStorage = MIO BuffBytes BuffSizeDiff

-- | Storage-mitigated type.
type StorageMitigated = Mitigated BuffBytes

instance Mitigator BuffBytes BuffSizeDiff where
  newMState q = do
    lock <- liftMIO newLock
    return MState { mState = Nothing, mQuant = q, mLock = lock }
  mitigate (Mitigated nr x) m = m x

give :: MonadMIO m => StorageMitigated a -> BS.ByteString -> MIOStorage m ()
give (Mitigated nr x) bs = do
  ms <- getMIOState nr
  withLock ms $ do
    let s   = mState ms
        s'  = maybe bs (\x -> append x bs) (mState ms)
        ms' = ms { mState = s' }
        --TODO: continue here


  withLock ms = do
    liftMIO $ aquireLock (mLock ms)
    res <- m
    liftMIO $ releaseLock (mLock ms)
    return m

quant :: Integer -> BuffSizeDiff
quant = BuffSizeDiff
