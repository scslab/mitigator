-- | This module preesnts a general mitigator interface and a
-- mitigated IO monad @MIO@.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mitigator ( -- * Mitigators, and mitigated types
                   Mitigator(..)
                 , MitigatorState(..)
                 , Mitigated(..)
                 -- * IO related
                 , MIOState(..), MitNr
                 , MIO(..), get, put
                 , newEmptyMIOState
                 , runMIO, evalMIO
                 , MonadMIO(..)
                 -- * Working with internal state
                 , withMIOLock 
                 , getMIOState
                 , putMIOState
                 ) where


import Control.Applicative
import Control.Monad.State.Strict hiding (get, put)
import qualified Control.Monad.State.Strict as State
import Control.Concurrent
import Lock

-- | Mitigated typed. 
data Mitigated s a = Mitigated { mitNr  :: !MitNr -- ^ Mitigator type number
                               , mitVal :: !a     -- ^ Raw type
                               }

-- | State of Mitigator polymorphic in mitigator state and quentum.
data MitigatorState s q = MState { mState :: !(Maybe s) -- ^ Internal state
                                 , mQuant :: !q         -- ^ Quantum
                                 , mLock  :: Lock       -- ^ Lock on state
                                 }

-- | A mitigator.
class Mitigator s q | s -> q where
  -- | Create new mitigator state.
  newMState      :: MonadMIO m => q -> MIO s q m (MitigatorState s q)
  -- | Mitigator constructor.
  mitigateC :: MonadMIO m => q -> m a -> MIO s q m (Mitigated s a)
  mitigateC q m = do lm <- lift m
                     nr <- withMIOLock $ do
                       s <- get
                       mstate <- newMState q
                       let nr = 1 + mioNr s 
                           ms = (nr, mstate) : mioMs s
                           s' = s { mioNr = nr, mioMs = ms}
                       put s'
                       return nr
                     return $ Mitigated nr lm

  -- | Mitigate function.
  mitigate :: MonadMIO m => Mitigated s a -> (a -> MIO s q m b) -> MIO s q m b


--
-- Mitigated computations
--

-- | Mitigator number.
type MitNr = Int

-- | Internal state of a mitigated computation, containing the
-- number of mitigators and their state.
data MIOState s q = MIOState { -- | Mitigators
                               mioMs   :: ![(MitNr, MitigatorState s q)]
                               -- | Number of mitigators
                             , mioNr   :: !MitNr
                               -- | State lock
                             , mioLock :: Lock
                             }

newtype MIO s q m a = MIO { unMIO :: StateT (MIOState s q) m a }
    deriving (Functor, Applicative, Monad, MonadTrans)

-- | Get global mitigator state.
get :: (Monad m) => MIO s q m (MIOState s q)
get = MIO $ State.get

-- | Update global mitigator state.
put :: (Monad m) => MIOState s q -> MIO s q m ()
put = MIO . State.put 

--
-- IO related
--


-- | Run @MIO@.
runMIO :: (MonadIO m) => MIO s q m a -> MIOState s q -> m (a, MIOState s q)
runMIO io s = runStateT (unMIO io) s


-- | Evaluate @MIO@ action with given state
evalMIO :: (MonadIO m) => MIO s q m a -> m a
evalMIO io = do
  s <- newEmptyMIOState
  liftM fst $ runMIO io s

-- | New empty "MIOState"
newEmptyMIOState :: MonadIO m => m (MIOState s q)
newEmptyMIOState = liftIO $ do
  lock <- newLock
  return (MIOState {mioMs = [], mioNr = 0, mioLock = lock} )

-- | Similar to @MonadIO@.
class Monad m => MonadMIO m where 
  liftMIO :: IO a -> MIO s q m a -- ^ Arbitrary IO operations in underlying monad.

instance MonadMIO IO where 
  liftMIO a = MIO . StateT $ \s -> a >>= \r -> return (r, s)


--
-- Working with state
--


-- | Execute an MIO action atomic w.r.t state.
withMIOLock :: MonadMIO m => MIO s q m a -> MIO s q m a
withMIOLock io = do
  s <- get -- Get state
  let lock = mioLock s -- lock
  liftMIO $ aquireLock lock 
  put (s { mioLock = error "FATAL: Don\'t touch the lock" }) -- Remove lock
  res <- io -- Execute action
  s' <- get -- Get new state
  put (s' { mioLock = lock }) -- Put lock back
  liftMIO $ releaseLock lock
  return res

-- | Get the state of a mitigator based on the number.
getMIOState :: MonadMIO m => MitNr -> MIO s q m (MitigatorState s q)
getMIOState nr = withMIOLock $ do
  s <- get
  let ms = mioMs s
  case lookup nr ms of
    Nothing -> fail $ "BUG: getMIOState did not find mitigator " ++ show nr
    Just m -> return m

-- | Update the state of the mitigator, based on the number.
putMIOState :: MonadMIO m => MitNr -> (MitigatorState s q) -> MIO s q m ()
putMIOState nr newState = withMIOLock $ do
  s <- get
  let ms  = mioMs s
      ms' = map (\(n, s) -> if n == nr then (n, newState) else (n, s)) ms
  case lookup nr ms of
    Nothing -> fail $ "BUG: putMIOState did not find mitigator " ++ show nr
    Just _ -> put (s {mioMs = ms'})
