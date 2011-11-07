{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Mitigated IO Core and basic mitigation combinators

module MIO where

import Control.Applicative
import Control.Monad.State hiding (get, put)
import Control.Concurrent
import System.Posix.Unistd
import Lock

--TODO: REMOVE:
import System.IO
import System.Posix.Clock
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
--

type MitNr = Int

-- | A mitigated typed. 
newtype Mitigated a = Mitigated { unMitigated :: (MitNr, a) }

-- | Time stamp.
type TStamp = Integer

-- | Time stamp difference in picoseconds.
type TStampDiff = Integer

-- | Mitigator state.
data MState = MState { mTStamp    :: !(Maybe TStamp) -- ^ Time stamp
                     , mQuantSize :: TStampDiff      -- ^ Lock on type
                     , mLock      :: Lock            -- ^ Lock on type
                     }

-- | Create a new mitigator state.
newEmptyMState :: MonadMIO m => TStampDiff -> MIO m (MState)
newEmptyMState q = do
  lock <- liftMIO newLock
  return MState { mTStamp = Nothing, mQuantSize = q, mLock = lock }


-- | Internal state of a mitigated computatoin, containing the
-- number of mitigators and their state.
data MIOState = MIOState { mioMs   :: ![(MitNr, MState)] -- ^ Mitigators
                         , mioNr   :: !MitNr           -- ^ Number of mitigators
                         , mioLock :: Lock             -- ^ State lock
                         }

newtype MIO m a = MIO (StateT MIOState m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadFix)

get :: (Monad m) => MIO m (MIOState)
get = MIO . StateT $ \s -> return (s, s)

put :: (Monad m) => MIOState -> MIO m ()
put s = MIO . StateT $ \_ -> return ((), s)

class Monad m => MonadMIO m where 
  liftMIO :: IO a -> MIO m a -- ^ Arbitrary IO operations in underlying monad.

instance MonadMIO IO where 
  liftMIO a = MIO . StateT $ \s -> a >>= \r -> return (r, s)

unMIO :: (MonadMIO m) => MIO m a -> MIOState -> m (a, MIOState)
unMIO (MIO (StateT f)) = f

newEmptyMIOState :: MonadIO m => m MIOState
newEmptyMIOState = liftIO $ do
  lock <- newLock
  return (MIOState {mioMs = [], mioNr = 0, mioLock = lock} )

runMIO :: (MonadIO m) => MIO m a -> m (a, MIOState)
runMIO (MIO (StateT f)) = do
  s <- newEmptyMIOState
  f s

-- | Execute an MIO action atomic w.r.t state.
withMIOLock :: MonadMIO m => MIO m a -> MIO m a
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
getMState :: MonadMIO m => MitNr -> MIO m MState
getMState nr = withMIOLock $ do
  s <- get
  let ms = mioMs s
  case lookup nr ms of
    Nothing -> fail $ "BUG: getMState did not find mitigator " ++ show nr
    Just m -> return m

-- | Update the state of the mitigator, based on the number.
putMState :: MonadMIO m => MitNr -> MState -> MIO m ()
putMState nr newState = withMIOLock $ do
  s <- get
  let ms  = mioMs s
      ms' = map (\(n, s) -> if n == nr then (n, newState) else (n, s)) ms
  case lookup nr ms of
    Nothing -> fail $ "BUG: putMState did not find mitigator " ++ show nr
    Just _ -> put (s {mioMs = ms'})
    

-- | This function is used to produce a mitigated communication point
-- (e.g., a @Handle@ or @Socket@
mitigateC :: MonadMIO m => TStampDiff -> m a -> MIO m (Mitigated a)
mitigateC q m = do lm <- lift m
                   nr <- withMIOLock $ do
                     s <- get
                     mstate <- newEmptyMState q
                     let nr = 1 + mioNr s 
                         ms = (nr, mstate) : mioMs s
                         s' = s { mioNr = nr, mioMs = ms}
                     put s'
                     return nr
                   return $ Mitigated (nr, lm)

getTStamp :: MonadMIO m => MIO m Integer
getTStamp = liftMIO $ do
  (TimeSpec s n) <- getTime Monotonic
  return $ ((fromIntegral s) * secInNano)  + (fromIntegral n)


mitigate :: MonadMIO m => Mitigated a -> (a -> m b) -> MIO m b
mitigate (Mitigated (nr,x)) m = do
  ms <- getMState nr 
  t1 <- getTStamp
  let q = mQuantSize ms
      t0 = case mTStamp ms of
             Nothing -> t1-q
             Just t -> t
      delta = t1 - t0
      q' = if delta <= q then q else q*2
  when (delta <  q) $ liftMIO . nanosleep $ (q - delta)
  t2 <- getTStamp
  putMState nr (ms { mQuantSize = q', mTStamp = Just t2 })
  lift (m x)

milliInNano :: Integer
milliInNano = 1000000

secInNano :: Integer
secInNano = 1000000000

openFile' :: FilePath -> IOMode -> MIO IO (Mitigated Handle)
openFile' f mode = mitigateC (10 * milliInNano) $ openFile f mode

hPut' :: Mitigated Handle -> BS.ByteString -> MIO IO ()
hPut' mH bs = mitigate mH $ \h -> BS.hPut h bs

hFlush' :: Mitigated Handle -> MIO IO ()
hFlush' (Mitigated (_,h)) = liftMIO $ hFlush h

hClose' :: Mitigated Handle -> MIO IO ()
hClose' (Mitigated (_,h)) = liftMIO $ hClose h

test = runMIO $ do
  h <- openFile' "/tmp/woo" WriteMode
  sequence_ $ replicate 5 $ hPut' h (BS.pack "hello world") >> hFlush' h
  liftMIO $ putStrLn "about to miss" >> sleep 2 >> putStrLn "DONE!"
  hPut' h (BS.pack "hello world") >> hFlush' h
  sequence_ $ replicate 5 $ hPut' h (BS.pack "hello world") >> hFlush' h
  hClose' h

