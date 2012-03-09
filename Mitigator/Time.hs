{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- | General interface to a time-mitigated IO monad.

module Mitigator.Time {-( MIOTime, TimeMitigated
                      , secToNano, milliToNano 
                      , quant
                      ) -}where

import Data.Maybe
import qualified Data.Map as Map

import System.Posix.Clock

import MonadConcur
import Mitigator
import Control.Monad.State.Strict
import Control.Concurrent ( threadDelay )

--import Debug.Trace


--
-- Time stamps and time stamp difference
--


--  Types


-- | Time stamp. Used as the mitigator internal state.
newtype TStamp = TStamp { unTStamp :: Integer }
  deriving (Eq, Ord, Num)

instance Show TStamp where
  show = show . unTStamp

-- | Time stamp difference in picoseconds. Used as the mitogator quote.
newtype TStampDiff = TStampDiff { unTStampDiff :: Integer }
  deriving (Eq, Ord, Num)

instance Show TStampDiff where
  show = show . unTStampDiff

-- | Type of mitigated monad.
type TimeMitM = MitM TStamp TStampDiff

-- | Time-mitigated handle.
type TimeMitigated = Mitigated TStamp


--  Operations

-- | Simple conversion to a time stamp.
class ToTStamp a where
  -- ^ Conversion to a time stamp.
  toTStamp :: a -> TStamp

-- | Simple conversion to a time stamp difference.
class ToTStampDiff a where
  -- ^ Conversion to a time stamp difference.
  toTStampDiff :: a -> TStampDiff

instance ToTStamp     TStamp     where toTStamp     = id
instance ToTStamp     TStampDiff where toTStamp     = TStamp . unTStampDiff
instance ToTStampDiff TStampDiff where toTStampDiff = id
instance ToTStampDiff TStamp     where toTStampDiff = TStampDiff . unTStamp

-- | Compute time difference.
class DiffTStamp a b where
  -- ^ Given two time stamps (or time stamp differences) compute the
  -- difference.
  tStampDiff :: a -> b -> TStampDiff

instance DiffTStamp TStamp TStamp where
  tStampDiff x y = toTStampDiff $ x - y
instance DiffTStamp TStampDiff TStampDiff where
  tStampDiff = (-)
instance DiffTStamp TStamp TStampDiff where
  tStampDiff x y = toTStampDiff x - y
instance DiffTStamp TStampDiff TStamp where
  tStampDiff x y = x - toTStampDiff y


--
-- Time mitigated monad
--

-- | Mitigator initial quantum in microseconds.
mkQuant :: Integer -> TStampDiff
mkQuant = TStampDiff 


instance (MonadConcur m, MonadTime m) => Mitigator m TStamp TStampDiff where
  mitigateWrite (Mitigated nr x) m = do
    -- Get current time stamp
    t1 <- lift getTStamp
    -- Get mitigator state and MVar holding it:
    s <- getMitMState
    let mvar = mioMs s Map.! nr
    ms   <- lift $ takeMVar mvar
    let q  = mQuant ms
        -- ^ Current quantum
        t0 = fromMaybe (toTStamp $ t1 `tStampDiff` q) $ mState ms 
        -- ^ Last time stamp
        delta = t1 `tStampDiff` t0
        -- ^ Difference between "now" and last time
        factor = unTStampDiff delta `div` unTStampDiff q + 1
        q' = TStampDiff . (if delta <= q then id else (^factor)) $ unTStampDiff q
        -- ^ If we did not meet the schedule, double the quota
    t1New <- lift getTStamp
    --when (delta > q) $ trace ("factor = "++ show factor ++ " new q = " ++ show q') (return ())
    -- ^ get another time stamp, takeMitigatorState have blocked
    let deltaNew = t1New `tStampDiff` t0
    --when (delta < q) $ lift $ putStrLn "sleeping" 
    --when (delta > q) $ lift $ putStrLn $ "doubling q to " ++ show q'
    lift $ fork $ do when (deltaNew < q) $ microSleep $ q `tStampDiff` deltaNew
                     -- ^ Sleep if we still have room in quota
                     m x
                     -- ^ Execute action
                     t2 <- getTStamp
                     -- ^ Get new timestamp
                     putMVar mvar $ ms { mQuant = q', mState = Just t2 }
                     -- ^ Update state

-- | Class of monads that can measure time.
class Monad m => MonadTime m where
  -- | Get time stamp in microseconds.
  getTStamp :: m TStamp
  -- | Sleep for a specified duration (in microseconds)
  microSleep :: TStampDiff -> m ()

-- | Default IO instance.
instance MonadTime IO where
  getTStamp = do
    (TimeSpec s n) <- getTime Realtime -- Monotonic
    return . TStamp . nanoToMicro $
                        secToNano (fromIntegral s)  + fromIntegral n
  microSleep = threadDelay . fromInteger . unTStampDiff

--
-- Misc unit conversions
--

-- | Convert seconds to nanoseconds.
secToNano :: Integer -> Integer
secToNano = (1000000000*)

-- | Convert milliseconds to nanoseconds
milliToNano :: Integer -> Integer
milliToNano = (1000000*)

-- | Convert microseconds to nanoseconds
microToNano :: Integer -> Integer
microToNano = (1000*)

nanoToMicro :: Integer -> Integer
nanoToMicro = (`div` 1000)
