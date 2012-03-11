{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- | General interface to a time-mitigated monads.
-- The first 'TimeMitM' and corresponding 'TimeMitigated' handle
-- mitigate inputs to the underlying computations (which themselves
-- may block for indefinite amount of time) -- an example use case
-- is the mitigation of file handles or sockets.
--
-- Conversely, the 'TimeMitMC' and corresponding 'TimeMitigatedC' 
-- mitigate computations and not the input to the computation. An
-- example use case is an web application that produces the
-- message-body which is itself sent to the client by trusted code.
-- In this case, the app itself needs to be mitigated (unless 
-- a separate \"handle\" corresponding to the app accross different
-- requests is kept).

module Mitigator.Time ( -- * Time migator
                        TimeMitM
                      , TimeMitigated
                      , mkQuant
                      , wait
                        --  * Computation mitigator
                      , TimeMitMC
                      , TimeMitigatedC
                        -- * Time related
                      , MonadTime(..)
                      , TStamp(..)
                      , TStampDiff(..)
                      ) where

import Data.Maybe
import qualified Data.Map as Map

import System.Posix.Clock

import MonadConcur
import Mitigator
import Control.Monad.State.Strict
import Control.Concurrent ( threadDelay )

#define DEBUG
#ifdef DEBUG
import Debug.Trace
#endif


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

-- | Type of time-mitigated monad.
type TimeMitM = MitM TStamp TStampDiff

-- | Time-mitigated handle.
type TimeMitigated = Mitigated TStamp

-- | Type of time-mitigated computation monad.
type TimeMitMC = MitM () TStampDiff

-- | Time-mitigated computation.
type TimeMitigatedC = Mitigated ()

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

-- | Same as mitigateWrite, but does no mitigation. This should be
-- used by functions that need to block on the last operation on
-- mitigated handle.
wait :: (MonadConcur m, MonadTime m)
     => Mitigated s a
     -> (a -> m ())
     -> MitM s q m ()
wait (Mitigated nr h) m = do
  mvar <- getMitMState >>= \s -> return $ mioMs s Map.! nr
  lift . fork $ do ms <- takeMVar mvar
                   m h
                   putMVar mvar ms

-- | Instance for mitigating handles, or conceputally inputs to computation.
instance (MonadConcur m, MonadTime m) => Mitigator m TStamp TStampDiff where
  mitigate (Mitigated nr h) m = do
    t1 <- lift getTStamp
    -- Get current time stamp
    mvar <- getMitMState >>= \s -> return $ mioMs s Map.! nr
    -- Get mitigator state and MVar holding it:
    lift $ fork $ do
      ms   <- takeMVar mvar
      let q  = mQuant ms
          -- Current quantum
          t0 = fromMaybe t1 $ mState ms 
          -- Last time stamp
          (delta, q') = computeNewQuantum t1 t0 q
          -- If we did not meet the schedule, double the quota
      t1New <- getTStamp
      -- Get another time stamp, takeMitigatorState have blocked
#ifdef DEBUG
      trace ("nr = "++ show nr 
             ++ "  q = " ++ show  q
             ++ "  t0 = " ++ show  t0
             ++ "  t1 = " ++ show  t1
             ++ "  delta = " ++ show  delta
             ++ "  q' = " ++ show q'
             ) (return ())
#endif
      let deltaNew = t1New `tStampDiff` t0
#ifdef DEBUG
      when (delta < q) $
        trace ("sleeping " ++ show nr ++ "for "
                 ++ show (q `tStampDiff` deltaNew)) (return ())
#endif
      when (deltaNew < q) $ microSleep $ q `tStampDiff` deltaNew
      -- Sleep if we still have room in quota
      m h
      -- Execute action
      t2 <- getTStamp
      -- Get new timestamp
      putMVar mvar $ ms { mQuant = q', mState = Just t2 }
      -- Update state

-- | Instance for mitigating computation.
instance (MonadConcur m, MonadTime m) => Mitigator m () TStampDiff where
  mitigate (Mitigated nr h) m = do
    mvar <- getMitMState >>= \s -> return $ mioMs s Map.! nr
    -- Get mitigator state and MVar holding it:
    lift $ fork $ do
      ms <- takeMVar mvar
      t0 <- getTStamp
      m h
      t1 <- getTStamp
      let q  = mQuant ms
          -- Last time stamp
          (delta, q') = computeNewQuantum t1 t0 q
          -- If we did not meet the schedule, double the quota
      when (delta < q) $ microSleep $ q `tStampDiff` delta
      putMVar mvar $ ms { mQuant = q' }
      -- Update state

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

-- | Compute the time difference and new quantum.
computeNewQuantum :: TStamp     -- ^ New time
                  -> TStamp     -- ^ Old time
                  -> TStampDiff -- ^ Current quantum
                  -> (TStampDiff, TStampDiff)
computeNewQuantum t1 t0 q =
  let d = t1 `tStampDiff` t0
      factor = unTStampDiff d `div` unTStampDiff q
      q' = TStampDiff . (if d <= q then id else (*(2^factor))) $ unTStampDiff q
   in (d, q')

-- | Convert seconds to nanoseconds.
secToNano :: Integer -> Integer
secToNano = (1000000000*)

-- | Convert nanoseconds to microseconds
nanoToMicro :: Integer -> Integer
nanoToMicro = (`div` 1000)
