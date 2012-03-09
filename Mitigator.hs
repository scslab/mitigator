{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
-- | This module preesnts a general mitigator interface and a
-- mitigated monad @MitM@.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mitigator ( -- * Mitigators and mitigated \"handles\"
                   Mitigated(..)
                 , Mitigator(..)
                 -- * Mitigator monad
                 , MitMState(..)
                 , MitNr
                 , MitigatorState(..)
                 , MitM(..)
                 , newEmptyMitMState
                 , runMitM
                 , evalMitM
                 -- * Internal
                 , withMitigator
                 , takeMitigatorState
                 , putMitigatorState
                 , getMitMState
                 , putMitMState
                 ) where


import Control.Applicative
import Control.Monad.State.Strict hiding (get, put)
import qualified Control.Monad.State.Strict as State
import MonadConcur

import Data.Map (Map)
import qualified Data.Map as Map

-- | Mitigated typed. The tye variable @s@ corresponds to the
-- mitigation type (e.g., time, or storage).
data Mitigated s a = Mitigated { mitNr  :: !MitNr -- ^ Mitigator number
                               , mitVal :: !a     -- ^ Handle
                               }

-- | State of Mitigator polymorphic in mitigator state and quentum.
data MitigatorState s q = MitigatorState { mState :: !(Maybe s)
                                           -- ^ Internal state
                                         , mQuant :: !q
                                           -- ^ Quantum
                                         }

-- | A mitigator parametrized by the mitigator internal state type and
-- quantum type.
class MonadConcur m => Mitigator m s q | s -> q where
  -- | Mitigate write function.
  mitigateWrite :: Mitigated s a    -- ^ Mitigated \"handle\"
                -> (a -> m ())      -- ^ Computation on handle to mitigate
                -> MitM s q m ()

  -- | Create a 'Mitigated' \"handle\".
  mkMitigated :: Maybe s  -- ^ Internal state
              -> q        -- ^ Quantum
              -> m a      -- ^ Handle constructor
              -> MitM s q m (Mitigated s a)
  mkMitigated mstate quant constr = do
    h <- lift constr
    s <- getMitMState
    let nr = 1 + mioNr s
    mit <- lift . newMVar $ MitigatorState { mState = mstate
                                           , mQuant = quant }
    putMitMState s { mioNr = nr, mioMs = Map.insert nr mit $ mioMs s }
    return $ Mitigated nr h



--
-- Mitigated computations
--

-- | Mitigator number
type MitNr = Int

-- | Internal state of a mitigated computation, containing
-- the mitigators (map from mitigator number to mitigator state),
-- total number of mitigators, and lock on the state.
data MitMState s q = MitMState { mioMs :: Map MitNr (MVar (MitigatorState s q))
                                 -- ^ Mitigators
                               , mioNr :: !MitNr
                                 -- ^ Number of mitigators
                               }

-- | Mitigatated monad.
newtype MitM s q m a = MitM { unMitM :: StateT (MitMState s q) m a }
    deriving (Functor, Applicative, Monad, MonadTrans)

-- | Get global mitigator state.
getMitMState :: MonadConcur m => MitM s q m (MitMState s q)
getMitMState = MitM State.get

-- | Update global mitigator state.
putMitMState :: MonadConcur m => MitMState s q -> MitM s q m ()
putMitMState = MitM . State.put 

-- | Run @MitM@.
runMitM :: MonadConcur m => MitM s q m a -> MitMState s q -> m (a, MitMState s q)
runMitM io = runStateT (unMitM io)


-- | Evaluate @MitM@ action with given state
evalMitM :: MonadConcur m => MitM s q m a -> m a
evalMitM io = do
  (res, s) <- runMitM io newEmptyMitMState
  -- Wait for all threads to finish writing:
  mapM_ (takeMVar . snd) $ Map.toList $ mioMs s
  return res

-- | New empty "MitMState"
newEmptyMitMState :: MitMState s q
newEmptyMitMState = MitMState { mioMs = Map.empty, mioNr = 0 }

--
-- Working with state
--


-- | Get the state of a mitigator based on the mitigator number.
takeMitigatorState :: MonadConcur m => MitNr -> MitM s q m (MitigatorState s q)
takeMitigatorState nr = do
  s <- getMitMState
  lift . takeMVar $ mioMs s Map.! nr

-- | Update the state of the mitigator, based on the mitigator number.
putMitigatorState :: MonadConcur m => MitNr -> MitigatorState s q -> MitM s q m ()
putMitigatorState nr newState = do
  s <- getMitMState
  lift $ putMVar (mioMs s Map.! nr) newState

-- | Execute action with lock on mitigator
withMitigator :: MonadConcur m
              => MitNr
              -> (MitigatorState s q -> MitM s q m a)
              -> MitM s q m a
withMitigator nr io = do
  s <- getMitMState
  ms <- lift . takeMVar $ mioMs s Map.! nr
  res <- io ms
  lift $ putMVar (mioMs s Map.! nr) ms
  return res
