module Util (timedMain, getTStamp, secToNano) where
import System.Posix.Clock

timedMain :: IO () -> IO ()
timedMain m = do
  t0 <- getTStamp
  m
  t1 <- getTStamp
  putStrLn $ "duation= "++ (show $ t1 -t0) ++ " nanoseconds in seconds"
  

getTStamp :: IO Integer
getTStamp = do
  (TimeSpec s n) <- getTime Monotonic
  return $ (secToNano (fromIntegral s))  + (fromIntegral n)

-- | Convert seconds to nanoseconds.
secToNano :: Integer -> Integer
secToNano = (1000000000*)
