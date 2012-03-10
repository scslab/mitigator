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
        users    = [1..n]
    forM_ (attacker `delete` users) $ \target -> do 
      myTry $ forM_ users $ \guess -> do 
        res <- mkReq attacker target guess
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
