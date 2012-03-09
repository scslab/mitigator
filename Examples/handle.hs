module Main (main) where
import MIO.Handle 
import Mitigator
import Control.Monad.Trans
import Control.Concurrent ( threadDelay )

import qualified Data.ByteString.Char8 as BS

main = evalMitM $ do
  h <- openFile "/tmp/woz" ReadWriteMode
  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world")
  lift $ putStrLn "about to miss" >> threadDelay 1900 >> putStrLn "DONE!"
  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world")
--  hPut h (BS.pack "hello world") >> hFlush h
--  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world") >> hFlush h
--  lift $ putStrLn "starting read"
--  sequence_ $ replicate 15 $ hGet h 1 >>= \b -> lift $ BS.putStrLn b
  hClose h
