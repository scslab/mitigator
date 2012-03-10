{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LIO.IterLIO ( evalIterLIO
                   , liftIterLIO
                   , lioHttpServer
                   -- * Trusted code
                   , mkIterLIOTCB
                   ) where

import Prelude hiding (catch)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.IterIO
import Data.IterIO.Http
import Data.Monoid
import qualified System.IO as SIO
import Data.Time (getCurrentTime)
import System.IO.Error (isEOFError)

import LIO.LIO hiding (liftIO)
import LIO.TCB
import qualified LIO.MonadCatch as LIO
import qualified LIO.MonadLIO

import qualified Data.ByteString.Lazy as L
import Control.Applicative

import qualified Data.ListLike as LL
import Data.ByteString.Lazy.Internal (defaultChunkSize)

type L = L.ByteString

--instance (MonadLIO m l s, MonadTrans t, Monad (t m)) => MonadLIO (t m) l s where
--    liftLIO = lift . liftLIO

instance (Label l, MonadLIO m l s) => MonadLIO (Iter t m) l s where
    liftLIO m = Iter $ \c -> IterM $ liftLIO $ do
      result <- try m                           
      case result of
        Right ok -> return $ Done ok c
        Left se -> return $ case E.fromException se of
          Just e | isEOFError e -> Fail (IterEOFErr e) Nothing (Just c)
          _ -> Fail (IterException se) Nothing (Just c)
      where try a = catch (Right `liftM` a) (return . Left)


runIterLIO :: (ChunkData t, Label l) =>
              Iter t (LIO l s) a -> LIOstate l s -> Iter t IO (a, LIOstate l s)
runIterLIO m0 s0 = adaptIter (\a -> (a, s0)) adapt m0
    where adapt m1 = do
            (m2, s) <- liftIO $ runLIO m1 s0
            runIterLIO m2 s

evalIterLIO :: (ChunkData t, Label l) =>
               s -> Iter t (LIO l s) a -> Iter t IO (a, l)
evalIterLIO s0 m0 = do
  liftM (\(a,s) -> (a, lioL s)) $ runIterLIO m0 (newstate s0)
 where newstate s = LIOstate { labelState = s , lioL = lbot , lioC = ltop }

mkIterLIOTCB :: (ChunkData t, Label l) =>
                Iter t IO a -> Iter t (LIO l s) a
mkIterLIOTCB = adaptIterM rtioTCB

liftIterLIO :: (ChunkData t, MonadLIO m l s) => Iter t (LIO l s) a -> Iter t m a
liftIterLIO = adaptIterM liftLIO

instance Show (Labeled l a) where
  show _ = error "**SHOULD NOW USE SHOW**"

lioHttpServer :: (Label l, MonadLIO m l s)
              => HttpRequestHandler m ()
              -> HttpServerConf m
lioHttpServer handler =
  HttpServerConf { srvLogger = liftLIO . rtioTCB . SIO.hPutStrLn SIO.stderr
                 , srvDate = liftLIO . rtioTCB $ Just `liftM` getCurrentTime
                 , srvHandler = handler
                 }
