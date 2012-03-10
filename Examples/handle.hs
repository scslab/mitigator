module Main (main) where
import MIO.Handle 
import Mitigator
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent ( threadDelay )

import qualified Data.ByteString.Char8 as BS

main = evalMitM $ do
  lift $ putStrLn "Opening /tmp/h0"
  h0 <- openFile "/tmp/h0" WriteMode 1000000
  replicateM_ 5 $ hPut h0 (BS.pack "fast   h0\n")
  lift $ threadDelay 6000000 -- slow down writes to h0
  lift $ putStrLn "Opening /tmp/h1"
  h1 <- openFile "/tmp/h1" WriteMode 1000000
  replicateM_ 5 $ hPut h1 (BS.pack "fast   h1\n")
  replicateM_ 5 $ hPut h0 (BS.pack "slower h0\n")
  hClose h0
  hClose h1
