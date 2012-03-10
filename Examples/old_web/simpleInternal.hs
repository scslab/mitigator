module Main where

import Prelude hiding (catch, head, div)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Functor
import System.IO
import System.Posix.Files

import LIO.LIO hiding (hPutStrLn, liftIO)
import LIO.TCB (ioTCB)
import LIO.Concurrent
import LIO.Concurrent.LMVar
import LIO.Concurrent.LMVar.TCB (newLMVarTCB)
import LIO.DCLabel hiding (label)
import DCLabel.NanoEDSL
import DCLabel.PrettyShow
import DCLabel.TCB (createPrivTCB)
--import System.Timeout
import System.Posix.Unistd

--
import qualified Data.List as List
--

--low = newDC ("low") (<>)

-- | Create some fake database
fakeVarH :: IO (LMVar DCLabel Int)
fakeVarH = fst <$> (evalDC $ do newLMVarTCB ltop 1)

main = do
  mv <- fakeVarH
  _ <- forkIO $ threadDelay 100000 >> (evalDC $ thread0 mv) >> return ()
  _ <- forkIO $ threadDelay 100000 >> (evalDC $ thread1 mv) >> return ()
  q <- newQSem 0
  _ <- forkIO $ threadDelay 5000000 >> signalQSem q
  waitQSem q
  return ()
  
  where thread1 var = do
          toLabeled ltop $ do
            v <- readLMVar var
            if v == 1
              then ioTCB $ threadDelay 1000000 >> return ()
              else return ()
          ioTCB $ (putStrLn "1" >> System.IO.hFlush stdout)
        thread0 var = do
          toLabeled ltop $ do
            v <- readLMVar var
            return ()
          ioTCB $ (putStrLn "0" >> System.IO.hFlush stdout)

    

