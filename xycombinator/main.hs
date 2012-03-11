{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (head, min, max, id, div)
import qualified Prelude as P
import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent
import           Control.Exception (finally)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
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
import LIO.DCLabel hiding (name)
import DCLabel.NanoEDSL

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

handleRequest :: (MonadIO m) => IO.Handle -> Iter L m ()
handleRequest h = do
  req <- httpReqI
  resp <- runHttpRoute routing req
  enumHttpResp resp Nothing .|$ handleI h
  where routing = mconcat [ routeTop $ routeFn $ mkHandler welcome
                          , routeMap [ ("welcome", routeFn $ mkHandler welcome)
                                     , ("matches", routeFn $ mkHandler matches)
                                     , ("termination", routeFn $ mkHandler termination)
                                     ]
                          ]

mkHandler :: MonadIO m => (HttpReq () -> DC (HttpStatus, L))
          -> HttpRequestHandler m ()
mkHandler app req = do
  let mtch = matchRegex (mkRegex "id=([0-9]+)") $ S.unpack $ reqQuery req
  let browserL = maybe lpub (\(uid:[]) -> newDC (uid) (<>)) mtch
  ((stat, body), resL) <- liftIO $ evalDC $ do
                            LIO.setLabelTCB lpub
                            res <- app req
                            LIO.wguard browserL
                            return res
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
  let (Just mtch) = matchRegex (mkRegex "uid=([0-9]+)") $ S.unpack $ reqQuery req
  let uid = read $ P.head mtch
  let (Just mtch) = matchRegex (mkRegex "fid=([0-9]+)") $ S.unpack $ reqQuery req
  let fid = read $ P.head mtch

  LIO.toLabeled LIO.ltop $ do
    interested <- interestFor uid
    if fid `elem` interested then
      forever $ return ()
      else return ()
  let body = renderHtml $
               page $ do
                h2 $ "Hi guest attacker!"
                p $ do
                  "User with id "
                  toHtml $ show fid
                  " named "
                  toHtml $ users Map.! fid
                  " is not interested in user with id "
                  toHtml $ show uid
                  " named "
                  toHtml $ users Map.! uid
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
  forever $ acceptConnection listener

acceptConnection :: Net.Socket -> IO ()
acceptConnection listener = do
  (s, addr) <- Net.accept listener
  forkIO $ handleConnection s
  return ()

handleConnection :: Net.Socket -> IO ()
handleConnection s = do
  h <- Net.socketToHandle s IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  enumHandle' h |$ handleRequest h
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

