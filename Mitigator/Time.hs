{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | General interface to a time-mitigated IO monad.

module Mitigator.Time ( MIOTime, TimeMitigated
                      , secToNano, milliToNano 
                      , quant
                      ) where

import System.Posix.Unistd
import System.IO
import System.Posix.Clock
import Lock
import Mitigator
import Control.Monad.State.Strict hiding (get, put)

-- | Time stamp.
newtype TStamp = TStamp { unTStamp :: Integer }
  deriving (Eq, Ord, Num, Show)

-- | Time stamp difference in picoseconds.
newtype TStampDiff = TStampDiff { unTStampDiff :: Integer }
  deriving (Eq, Ord, Num, Show)

-- | Type of mitigated IO.
type MIOTime = MIO TStamp TStampDiff

-- | Time-mitigated type.
type TimeMitigated = Mitigated TStamp



-- | Simple conversion to and from time stamps and differences.
class ToTStamp a where toTStamp :: a -> TStamp
class ToTStampDiff a where toTStampDiff :: a -> TStampDiff

instance ToTStamp     TStamp     where toTStamp     = id
instance ToTStamp     TStampDiff where toTStamp     = TStamp . unTStampDiff
instance ToTStampDiff TStampDiff where toTStampDiff = id
instance ToTStampDiff TStamp     where toTStampDiff = TStampDiff . unTStamp

-- | Compute time difference.
class DiffTStamp a where tStampDiff :: a -> a -> TStampDiff
instance DiffTStamp TStamp where tStampDiff x y = TStampDiff . unTStamp $ x - y
instance DiffTStamp TStampDiff where tStampDiff = (-)


instance Mitigator TStamp TStampDiff where
  newMState q = do
    lock <- liftMIO newLock
    return MState { mState = Nothing, mQuant = q, mLock = lock }

  mitigate (Mitigated nr x) m = do
    ms <- getMIOState nr 
    t1 <- getTStamp
    let q = mQuant ms
        t0 = case mState ms of
               Nothing -> toTStamp $ t1 `tStampDiff` (toTStamp q)
               Just t -> toTStamp t
        delta = t1 `tStampDiff` t0
        q' = if delta <= q then q else q*2
    when (delta <  q) $ liftMIO . nanosleep . unTStampDiff $ (q `tStampDiff` delta)
    t2 <- getTStamp
    putMIOState nr (ms { mQuant = q', mState = Just t2 })
    lift (m x)


-- | Get time stamp.
getTStamp :: MonadMIO m => MIOTime m TStamp
getTStamp = liftMIO $ do
  (TimeSpec s n) <- getTime Monotonic
  return . TStamp $ (secToNano (fromIntegral s))  + (fromIntegral n)

-- | Convert seconds to nanoseconds.
secToNano :: Integer -> Integer
secToNano = (1000000000*)

-- | Convert milliseconds to nanoseconds
milliToNano :: Integer -> Integer
milliToNano = (1000000*)

-- | Mitigator initial quantum in nanoseconds.
quant :: Integer -> TStampDiff
quant = TStampDiff
