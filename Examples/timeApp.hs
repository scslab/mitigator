import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpClient

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8

import System.Posix.Clock

main :: IO ()
main = forever $ do
  time $ http "http://localhost:8000/app2"

http url = do resp <- simpleGetHttp url
              enumHttpResp resp |$ stdoutI

getTStamp = do
  (TimeSpec s n) <- getTime Realtime
  return . nanoToMicro $ secToNano (fromIntegral s)  + fromIntegral n

nanoToMicro :: Integer -> Integer
nanoToMicro = (`div` 1000)

secToNano :: Integer -> Integer
secToNano = (1000000000*)

microToSec :: Integer -> Integer
microToSec = (`div` 1000000)

time m = do
  t0 <- getTStamp
  m
  t1 <- getTStamp
  putStrLn $ "Duation= "++ (show $ t1 - t0) ++ " microseconds in seconds"
