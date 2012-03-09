module Main (main) where
import MIO.Handle 
import Mitigator
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent ( threadDelay )

import qualified Data.ByteString.Char8 as BS

main = evalMitM $ do
  h <- openFile "/tmp/woz" ReadWriteMode
  replicateM_ 5 $ hPut h (BS.pack "hello world")
  lift $ putStrLn "about to miss" >> threadDelay 1900 >> putStrLn "DONE!"
  replicateM_ 5 $ hPut h (BS.pack "hello world")
  hClose h
