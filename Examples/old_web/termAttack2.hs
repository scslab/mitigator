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
      _ <- mkDone attacker False
      mkReq attacker target
      r <- mkDone attacker True
      maybe (return ()) (\g -> putStrLn (show target ++ ":" ++ g)) r 
    where n = databaseSize
          myTry :: IO a -> IO (Either SomeException a)
          myTry = try
          mkU u = "user"++ show u
          --
          mkReq attacker target = do
            let url = "http://localhost:8000/app4"
                      ++ "?user=" ++ mkU attacker
                      ++ "&attack=" ++ mkU target
            simpleHTTP (getRequest url)
          --
          mkDone attacker doPrint = do
            let url = "http://localhost:8000/app4"
                      ++ "?user=" ++ mkU attacker
                      ++ "&done=1"
                      ++ if doPrint then "&print=1" else ""
            --putStrLn url
            r <- simpleHTTP (getRequest url)
            r' <- (take 100) <$> (getResponseBody r)
            return $ if "DONE=>" `isPrefixOf` r'
                       then Just $ drop 6 r'
                       else Nothing
                                     
