{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.IterIO
import           Data.IterIO.Http
import           Data.IterIO.HttpRoute
import           Data.IterIO.SSL
import qualified Data.ByteString.Lazy as L
import           Data.Monoid

import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent (forkIO)
import           Control.Concurrent.QSem
import           Control.Exception

import qualified Network.Socket as Net
import           OpenSSL
import qualified OpenSSL.Session as SSL
import           HttpServer

import           System.Posix.Files
import           System.IO

import           MonadConcur
import           Mitigator
import           Mitigator.Time


type L = L.ByteString

main :: IO ()
main = Net.withSocketsDo $ withOpenSSL $ do
  mctx <- if secure
          then do
            exists <- fileExist privkey
            unless exists $ genSelfSigned privkey "localhost"
            ctx <- simpleContext privkey
            return $ Just ctx
          else return Nothing
  srv <- mkHttpServer (if secure then 4433 else 8000) mctx
  sem <- newQSem 0
  _ <- forkIO $ evalMitM (acceptLoop srv) `finally` signalQSem sem
  waitQSem sem
    where
      privkey = "testkey.pem"
      secure = False -- True

acceptLoop :: HttpServer -> MIO ()
acceptLoop srv = do
  -- ^ Create route creates the mitigated app routes
  route <- createRoute
  loop $ runHttpRoute route
    where
      loop handler = do
        (s, addr) <- lift . Net.accept $ hsListenSock srv
        lift $ hPrint stderr addr
        _ <- forkMitM $ server handler srv s
        loop handler
      server handler srv s = lift $ do
        (iter, enum) <- maybe (iterStream s)
                              (\ctx -> iterSSL ctx s True)
                              (hsSslCtx srv)
        enum |$ inumHttpServer (ioHttpServer handler) .| iter

createRoute :: MIO (HttpRoute IO ())
createRoute = do
  handles <- sequence [ appHandle "app1" app1 5000000
                      , appHandle "app2" app2 1000000 
                      ]
  s <- getMitMState
  lift $ print s
  let routes = map (\mh -> (getName mh, routeAppHandle mh s)) handles
  return $ mconcat [ routeMap routes ]
    where getName (Mitigated _ (AppHandle n _)) = n

--routeAppHandle :: TimeMitigated AppHandle -> HttpRoute IO ()
routeAppHandle mh s = routeFn $ \req -> lift $ liftM fst $ flip runMitM s $ do
  mvar <- lift newEmptyMVar
  mitigateWrite mh $ \(AppHandle _ h) -> do
    body <- h req
    putMVar mvar body
  body <- lift $ takeMVar mvar
  return $ toHttpResp body
  where toHttpResp = mkHtmlResp stat200

-- | Data types for App handles.
data AppHandle = AppHandle String (HttpReq () -> IO L)

-- | Create an app handle
appHandle :: String
          -> (HttpReq () -> IO L)
          -> Integer
          -> MIO (TimeMitigated AppHandle)
appHandle n f q = mkMitigated Nothing (mkQuant q) $ return $ AppHandle n f

instance Show AppHandle where
  show (AppHandle s _) = s

app1 :: HttpReq s -> IO L
app1 _ = return "App1"

app2 :: HttpReq s -> IO L
app2 _ = return "App2"
-- | Time mitigated IO monad.
type MIO = TimeMitM IO 

