{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (catch, head, div)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe
import Data.Monoid
import Data.Functor
import qualified Network.Socket as Net
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import System.IO
import System.Posix.Files

import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute
import Data.IterIO.SSL

import LIO.LIO hiding (hPutStrLn, liftIO)
import LIO.TCB (ioTCB)
import qualified LIO.LIO as LIO
import LIO.Concurrent
import LIO.Concurrent.LMVar
import LIO.Concurrent.LMVar.TCB (newLMVarTCB)
import LIO.DCLabel hiding (label)
import DCLabel.NanoEDSL
import DCLabel.PrettyShow
import DCLabel.TCB (createPrivTCB)
import qualified System.Timeout as Timeout
import System.Posix.Unistd
import System.Random
import Config

--
import qualified Data.List as List
--


type L8 = L8.ByteString
type S8 = S8.ByteString
type L = L.ByteString

data HttpServer = HttpServer {
      hsListenSock :: !Net.Socket
    , hsSslCtx :: !(Maybe SSL.SSLContext)
    , hsLog :: !(Maybe Handle)
    }

myListen :: Net.PortNumber -> IO Net.Socket
myListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

mkServer :: Net.PortNumber -> Maybe SSL.SSLContext -> IO HttpServer
mkServer port mctx = do
  sock <- myListen port
  h <- openBinaryFile "http.log" WriteMode
  hSetBuffering h NoBuffering
  return $ HttpServer sock mctx (Just h)


-- | Very Simple key value store
type DB = [(Key, Value)]
-- | Keys are stings
type Key = String
-- | Values are bytestrings
type Value = LMVar DCLabel L
-- | Type of parameters
type Params = [(L8, L8)]

-- From: haskell.og 99questions/solutions/25:
rnd_permu :: [a] -> IO [a]
rnd_permu []     = return []
rnd_permu (x:xs) = do
    rand <- randomRIO (0, (length xs))
    rest <- rnd_permu xs
    return $ let (ys,zs) = splitAt rand rest
             in ys++(x:zs)

-- | Create some fake database
createFakeDB :: IO DB
createFakeDB = fst <$>
  (evalDC $ do
    let users=[1..n]
    pusers <- ioTCB $ rnd_permu users
    forM (zip users pusers) $ \(k,r) -> do
      v <- newLMVarTCB (mkL k) (mkD r)
      return (mkU k, v)
  )

  where n = databaseSize
        mkU k = "user" ++ show k
        mkL k = newDC (mkU k) (<>)
        mkD k = L8.pack $ mkU k


accept_loop :: HttpServer -> DB -> IO ()
accept_loop srv db = loop
    where
      loop = do
        (s, addr) <- Net.accept $ hsListenSock srv
        hPutStrLn stderr (show addr)
        _ <- forkIO $ server s
        loop
      server s = do
        (iter, enum) <- maybe (iterStream s)
                              (\ctx -> iterSSL ctx s True)
                              (hsSslCtx srv)
        let loger = maybe inumNop inumhLog $ hsLog srv
        enum |. loger |$ inumHttpServer (ioHttpServer (handler db)) .| loger .| iter
      handler db = runHttpRoute (route db)

main :: IO ()
main = Net.withSocketsDo $ SSL.withOpenSSL $ do
  mctx <- if secure
          then do
            exists <- fileExist privkey
            unless exists $ genSelfSigned privkey "localhost"
            ctx <- simpleContext privkey
            return $ Just ctx
          else return Nothing
  srv <- mkServer (if secure then 4433 else 8000) mctx
  db <- createFakeDB
  sem <- newQSem 0
  _ <- forkIO $ accept_loop srv db `finally` signalQSem sem
  waitQSem sem
    where
      privkey = "testkey.pem"
      secure = False -- True

--
-- Routes to app
--

route :: DB -> HttpRoute IO ()
route db = mconcat [ routeMap apps ]
  where apps = [ ("app1", routeFn (launchDCApp app1 db)) 
               , ("app2", routeFn (launchDCApp app2 db))
               , ("app3", routeFn (launchDCApp app3 db))
               , ("app4", routeFn (launchDCApp app4 db))
               ]


launchDCApp :: 
     (Maybe Key -> DCPrivTCB -> DB -> Params -> HttpReq s -> DC (HttpResp IO))
  -> DB -> HttpReq s
  -> Iter L.ByteString IO (HttpResp IO)
launchDCApp app db req = do
  params <- getParams req
  let (usr, privs) = getUserPrivs params
  liftIO $ do
    r <- catch (timeout timeoutPeriod $ fst <$> (evalDC $ app usr privs db params req))
               (\(e::SomeException) -> do putStrLn (show e)
                                          threadDelay timeoutPeriod
                                          return defaultResp)
    return $ maybe (fromJust defaultResp) id r
      where defaultResp :: Maybe (HttpResp IO)
            defaultResp = Just $ mkHtmlResp stat200 "TIMEOUT"

getUserPrivs :: Params -> (Maybe Key, DCPrivTCB)
getUserPrivs params =
  case lookup "user" params of
    Nothing -> (Nothing, noPriv)
    Just u -> (Just (L8.unpack u), createPrivTCB $ newPriv (L8.unpack u))

getParams :: Monad m => HttpReq s -> Iter L8 m Params
getParams req = let docontrol acc field = do
                    val <- pureI
                    return $ (L8.pack . S8.unpack . ffName $ field, val) : acc
                in foldForm req docontrol []

--
-- Internal timing attack
-- 

lookupParam :: Params -> L8 -> Maybe String
lookupParam params k =  do
  u <- lookup k params 
  return (L8.unpack u)


newtype Log = Log { unLog :: [(Key, Int)] }
  deriving (Eq,Show,Read)

-- Ignore "thread" no
instance Ord Log where
  (Log a) <= (Log b) = map fst a <= map fst b

restructure :: (Eq a, Eq b) => [(a,b)] -> [ (a,b,b) ]
restructure [] = []
restructure ((k,v):xs) = let (x@(_,v'):_) = filter ((==k) . fst) xs
                         in (k,v,v') : restructure (List.delete x xs)

findLeakFromLog :: Log -> [Key]
findLeakFromLog (Log xs) = map (\(k,_,_) -> k) $
  filter (\(_,x,y) -> x==1 && y ==0) $ restructure xs


-- | Internal timing attack
app1 :: Monad m 
     => Maybe Key -> DCPrivTCB -> DB -> Params -> HttpReq s -> DC (HttpResp m)
app1 usr p db params req = 
    maybe (return $ resp404 req) id (case lookup "done" params of
                                       Nothing -> doit
                                       Just _ ->  done
                                    )
  where done = do
          me <- usr
          me_val <- lookup me db 
          return $ do
            logS <- takeLMVarP p me_val
            putLMVarP p me_val (L8.pack . show $ Log [])
            if isJust $ (lookupParam params "print")
              then do --ioTCB $ L8.putStrLn (L8.append "FINAL LOG =" logS)
                      let log = read (L8.unpack logS)
                      reconstructed <- tryEval p (findLeakFromLog log)
                      let res = case reconstructed of
                                  Nothing ->  "FAIL"
                                  Just [] ->  "RETRY"
                                  Just (x:_) -> "DONE=>" ++ (show x)
                      return $ mkHtmlResp stat200 $ L8.pack $ res
              else return $ mkHtmlResp stat200 $ L8.pack $ "FAIL"
        doit = do
          me <- usr
          me_val <- lookup me db 

          target <- L8.unpack <$> (lookup "attack" params)
          target_val <- lookup target db 

          guess <- lookup "guess" params
          thread <- lookup "thread" params
          return $ do
            printStat p
            -- thread 1:
            when (thread == "1") $ do
              toLabeledOrlFork ltop $ do
                v <- readLMVarP p target_val
                if v == guess
                  then ioTCB $ threadDelay 30000
                  else return ()
            -- thread 0:
            when (thread == "0") $ ioTCB $ threadDelay 10000
            --
            logS <- takeLMVarP p me_val
            let (Log log) = read (L8.unpack logS)
                log' = Log $ (L8.unpack guess, if thread=="1" then 1 else 0):log
            putLMVarP p me_val (L8.pack . show $ log')
            -- done? :
            ioTCB $ threadDelay 20000
            logS <- readLMVarP p me_val
            let log = read (L8.unpack logS)
            reconstructed <- tryEval p (findLeakFromLog log)
            let res = case reconstructed of
                        Nothing ->  "RETRY"
                        Just [] ->  "RETRY"
                        Just (x:_) -> "DONE=>" ++ (show x)
            return $ mkHtmlResp stat200 $ L8.pack $ res
        tryEval p x = LIO.catchP p (Just <$> LIO.evaluate x)
                                   (\_ (_::SomeException) -> return Nothing)

-- | External timing attack
app2 :: Monad m 
     => Maybe Key -> DCPrivTCB -> DB -> Params -> HttpReq s -> DC (HttpResp m)
app2 usr p db params req = 
    maybe (return $ resp404 req) id doit
  where doit = do
          me <- usr
          me_val <- lookup me db 

          target <- L8.unpack <$> (lookup "attack" params)
          target_val <- lookup target db 

          return $ do
            printStat p
            let users = map fst db-- filter (\x -> x/=me && x/=target) $ map fst db
                sleepIncrement = 200000
            forM_ (zip [1..] users) $ \(no,guess) ->
              toLabeledOrlFork ltop $ do
                v <- readLMVarP p target_val
                if v == L8.pack guess
                  then ioTCB $ threadDelay (no*sleepIncrement)
                  else return ()

            return $ mkHtmlResp stat200 $ L8.pack $ show users

-- | Simple termination channel attack
app3 :: Monad m 
     => Maybe Key -> DCPrivTCB -> DB -> Params -> HttpReq s -> DC (HttpResp m)
app3 usr p db params req = 
    maybe (return $ resp404 req) id doit 
  where doit = do
          me <- usr
          me_val <- lookup me db 

          target <- L8.unpack <$> (lookup "attack" params)
          target_val <- lookup target db 

          guess <- lookup "guess" params
          return $ do
            printStat p
            toLabeledOrlFork ltop $ do
              v <- readLMVarP p target_val
              if v == guess
                then forever $ return ()
                else return ()
            return $ mkHtmlResp stat200 $ L8.pack $ "DONE"



-- | Termination channel attack with storage
app4 :: Monad m 
     => Maybe Key -> DCPrivTCB -> DB -> Params -> HttpReq s -> DC (HttpResp m)
app4 usr p db params req = 
    maybe (return $ resp404 req) id (case lookup "done" params of
                                       Nothing -> doit
                                       Just _ ->  done
                                    )
  where done = do
          me <- usr
          me_val <- lookup me db 
          return $ do
            logS <- takeLMVarP p me_val
            putLMVarP p me_val (L8.pack . show $ ([] :: [Key]))
            if isJust $ (lookupParam params "print")
              then let log :: [Key]
                       log = read (L8.unpack logS)
                       resp = if length log > 0
                                then "DONE=>" ++ (List.head $ log)
                                else "FAIL"
                   in return $ mkHtmlResp stat200 $ L8.pack $ resp
              else return $ mkHtmlResp stat200 $ L8.pack $ "FAIL"
        doit = do
          me <- usr
          me_val <- lookup me db 

          target <- L8.unpack <$> (lookup "attack" params)
          target_val <- lookup target db 

          return $ do
            printStat p
            let users = map fst db
            forM_ users $ \guess -> do
              logS <- takeLMVarP p me_val
              let log :: [Key]
                  log = read (L8.unpack logS)
                  log' = guess : log
              putLMVarP p me_val (L8.pack . show $ log')
              toLabeledOrlFork ltop $ do
                v <- readLMVarP p target_val
                if v == (L8.pack guess)
                  then forever $ return ()
                  else return ()
              --
            return $ mkHtmlResp stat200 $ L8.pack $ "RETRY"


-- 
-- DEBUG/SETTING
--
printStat p = when debug $ do
  l <- getLabel
  c <- getClearance
  ioTCB . putStr   $ " label = " ++ prettyShow l
  ioTCB . putStr   $ " clear = " ++ prettyShow c
  ioTCB . putStrLn $ " privs = " ++ prettyShow p
    where debug = False

toLabeledOrlFork l m = if fix
                        then lFork l m >> return ()
                        else toLabeled l m >> return ()
        where fix = False

timeout :: Int -> IO a -> IO (Maybe a)
timeout t m = if enabled
                then Timeout.timeout t m
                else Just <$> m
    where enabled = timeoutEnable
-- 
--
--
