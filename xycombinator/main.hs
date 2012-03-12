{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (head, min, max, id, div, catch)
import qualified Prelude as P
import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent
import           Control.Exception (finally)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List (intersperse)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as Net
import qualified System.IO as IO
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (title, form, label)
import           Text.Blaze.Renderer.Utf8
import           Text.Regex
import           Data.IterIO
import           Data.IterIO.Http
import           Data.IterIO.HttpRoute
import qualified Data.ListLike as LL

import qualified LIO.TCB as LIO
import LIO.MonadCatch
import LIO.Concurrent
import LIO.LIORef.TCB
import LIO.DCLabel hiding (name)
import System.Posix.Unistd
import DCLabel.NanoEDSL

import Mitigator
import Mitigator.Time

type MIO = TimeMitMC IO 
type L = L.ByteString
type S = S.ByteString

interests :: Map Integer (LIO.Labeled DCLabel [Integer])
interests = Map.fromList $ P.map (\(k,v) ->
                  (k, LIO.labelTCB (newDC (show k) (<>)) v)) lst
  where lst = [
                (1, [3, 5, 4, 10])
              , (2, [2, 7,5])
              , (3, [1])
              , (4, [5, 6, 10])
              , (5, [2, 4, 6])
              , (6, [9, 2, 1, 4, 5, 6])
              , (7, [3, 4, 5])
              , (8, [7])
              , (9, [8, 9, 2, 1, 7, 4, 3, 10])
              , (10, [1])
              ]

users :: Map Integer String
users = Map.fromList [
    (1, "Elnora Dolce")
  , (2, "Clinton Alli")
  , (3, "Nelson Gratz")
  , (4, "Selena Storlie")
  , (5, "Max Yonkers")
  , (6, "Neva Slatter")
  , (7, "Marcie Brinkmann")
  , (8, "Elnora Lagrange")
  , (9, "Javier Casimir")
  , (10, "Neil Hartigan")
  ]

port :: Net.PortNumber
port = 8000

--
-- Request handler
--

-- | Data types for App handles.
data AppHandle = AppHandle String (HttpReq () -> DC (HttpStatus, L))

-- | Create an app handle
appHandle :: String
          -> (HttpReq () -> DC (HttpStatus, L))
          -> Integer
          -> MIO (TimeMitigatedC AppHandle)
appHandle n f q = mkMitigated Nothing (mkQuant q) $ return $ AppHandle n f

handleRequest :: IO.Handle -> HttpRequestHandler IO () -> Iter L IO ()
handleRequest h handler = do
  req <- httpReqI
  resp <- handler req
  enumHttpResp resp Nothing .|$ handleI h

routeAppHandle :: TimeMitigatedC AppHandle
               -> MitMState () TStampDiff
               -> HttpRoute IO ()
routeAppHandle mh s = routeFn $ \req -> lift $ liftM fst $ flip runMitM s $ do
  mvar <- lift newEmptyMVar
  mitigate mh $ \(AppHandle _ h) -> do
    resp <- (mkHandler h) req
    putMVar mvar resp
  lift $ takeMVar mvar

routing :: MIO (HttpRoute IO ())
routing = do
  let mitTime = 500000
  handles <- sequence $ [ appHandle "welcome" (welcome) mitTime
                , appHandle "matches" (matches) mitTime
                , appHandle "termination" (termination) mitTime
                , appHandle "internal" (internal) mitTime
                , appHandle "external" (external) mitTime]

  s <- getMitMState
  let routes = P.map (\mh -> (getName mh, routeAppHandle mh s)) handles
  return $ mconcat [ routeTop $ routeConst $ resp303 "/welcome"
                   , routeMap routes]
  where getName (Mitigated _ (AppHandle n _)) = n

mkHandler :: MonadIO m => (HttpReq () -> DC (HttpStatus, L))
          -> HttpReq () -> IO (HttpResp m)
mkHandler app req = do
  let mtch = matchRegex (mkRegex "id=([0-9]+)") $ S.unpack $ reqQuery req
  let browserL = maybe lpub (\(uid:[]) -> newDC (uid) (<>)) mtch
  ((stat, body), resL) <- liftIO $ evalDC $ do
                          catch (do
                            LIO.setLabelTCB lpub
                            res <- app req
                            LIO.wguard browserL
                            return res)
                            (\e@(LIO.LerrHigh) -> return (stat500, L.pack $ show e ++ "\n"))
  return $ mkHtmlResp stat body


interestFor :: Integer -> DC [Integer]
interestFor uid = LIO.unlabel $ interests Map.! uid

--
-- Controllers
--

page :: Html -> Html
page bd = docTypeHtml $ do
  head $ do
    title $ "XYCombinator"
  body $ do
    header $ do
      h1 $ a ! href "/" $ "XYCombinator"
      h2 $ do "get your "; em "thunk"; " on"
    bd

termination :: HttpReq () -> DC (HttpStatus, L)
termination req = do
  let (Just mtch) = matchRegex (mkRegex "id1=([0-9]+)") $ S.unpack $ reqQuery req
  let uid = read $ P.head mtch
  let (Just mtch) = matchRegex (mkRegex "id2=([0-9]+)") $ S.unpack $ reqQuery req
  let fid = read $ P.head mtch

  -- THIS IS THE IMPORTANT PART
  LIO.toLabeled LIO.ltop $ do
    interested <- interestFor uid
    if fid `elem` interested then
      forever $ return ()
      else return ()
  -- THIS IS THE IMPORTANT PART

  let body = renderHtml $ "Hi attacker! You guessed wrong..."
  return $ (stat200, body)

sleepLIO :: LIO.LabelState l p s => Int -> LIO.LIO l p s Int
sleepLIO t = LIO.ioTCB $ sleep t

internal :: HttpReq () -> DC (HttpStatus, L)
internal req = do
  let (Just mtch) = matchRegex (mkRegex "id1=([0-9]+)") $ S.unpack $ reqQuery req
  let uid = read $ P.head mtch
  let (Just mtch) = matchRegex (mkRegex "id2=([0-9]+)") $ S.unpack $ reqQuery req
  let fid = read $ P.head mtch

  -- THIS IS THE IMPORTANT PART
  shared <- newLIORef lpub [] -- Simulates a persistent datastore

  refHi <- lFork lpub $ do
    LIO.toLabeled LIO.ltop $ do
      interested <- interestFor uid
      if fid `elem` interested then do
        sleepLIO 5
        return ()
        else return ()
    modifyLIORef shared (1:)
  refLow <- lFork lpub $ do
    sleepLIO 3
    modifyLIORef shared (0:)

  lWait refHi
  lWait refLow
  -- THIS IS THE IMPORTANT PART
  result <- readLIORef shared
  let body = renderHtml $ toHtml $ show result
  return $ (stat200, body)

external :: HttpReq () -> DC (HttpStatus, L)
external req = do
  let (Just mtch) = matchRegex (mkRegex "id1=([0-9]+)") $ S.unpack $ reqQuery req
  let uid = read $ P.head mtch
  let (Just mtch) = matchRegex (mkRegex "id2=([0-9]+)") $ S.unpack $ reqQuery req
  let fid = read $ P.head mtch

  -- THIS IS THE IMPORTANT PART
  interested <- interestFor uid
  if fid `elem` interested then do
    sleepLIO 1
    return ()
    else return ()
  -- THIS IS THE IMPORTANT PART

  let body = renderHtml $ "done"
  return $ (stat200, body)

matches :: HttpReq () -> DC (HttpStatus, L)
matches req = do
  let (Just mtch) = matchRegex (mkRegex "id=([0-9]+)") $ S.unpack $ reqQuery req
  let uid = read $ P.head mtch
  let me = users Map.! (uid)
  interested <- interestFor uid
  let num = show $ length $ interested
  let body = renderHtml $
               page $ do
                h2 $ do "Hi "; toHtml me
                p $ do "There are "; toHtml num; " people interested in you:"
                ol $
                  forM_ interested (\iid -> li $ toHtml $ users Map.! iid)
                h3 "Try a different user id"
                form ! action "/matches" ! method "GET" $ do
                  div $ do
                    label ! for "id" $ "Enter your user-id (1-10):"
                    br
                    input ! type_ "number" ! min "1" !
                            max "10" ! name "id" ! id "id"
                  input ! type_ "submit" ! value "Check matches!"
  return $ (stat200, body)

welcome :: HttpReq () -> DC (HttpStatus, L)
welcome req = do
  let body = renderHtml $
               page $ do
                p $ "At XYCombinator, we believe that no function \
                    \should be left unapplied. Join us, and let us \
                    \help you find your fixed point!"
                h2 "Find out who's interested in you:"
                form ! action "/matches" ! method "GET" $ do
                  div $ do
                    label ! for "id" $ "Enter your user-id (1-10):"
                    br
                    input ! type_ "number" ! min "1" !
                            max "10" ! name "id" ! id "id"
                  input ! type_ "submit" ! value "Check matches!"
  return $ (stat200, body)

--
-- Server
--

main :: IO ()
main = Net.withSocketsDo $ do
  listener <- myListen port
  forever $ evalMitM $ acceptConnection listener

acceptConnection :: Net.Socket -> MIO ()
acceptConnection listener = do
  (s, addr) <- lift $ Net.accept listener
  rts <- routing
  forkMitM $ handleConnection s $ runHttpRoute rts
  return ()

handleConnection :: Net.Socket -> HttpRequestHandler IO () -> MIO ()
handleConnection s rts = do
  lift $ do
    h <- Net.socketToHandle s IO.ReadWriteMode
    IO.hSetBuffering h IO.NoBuffering
    enumHandle' h |$ handleRequest h rts
    IO.hClose h


--
-- Utilities
--

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

