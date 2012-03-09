-- | Simple time-mitigated interface  IO with handles
-- Note that reads are not mitigated as on most real systems you can
-- disable @atime@.
module MIO.Handle ( FilePath
                  , Handle
                  , openFile
                  , IOMode(..)
                  , hClose
                  , hPut
                  , hPutStrLn
                  , hGetLine
                  , hGetContents
                  , hGet
                  , hIsEOF
                  , hIsOpen
                  , hIsClosed
                  , hIsReadable
                  , hIsWriteable
                  ) where
import Mitigator
import Mitigator.Time
import System.IO (IOMode(..), BufferMode(..))
import qualified System.IO as SIO
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans

type Handle = TimeMitigated SIO.Handle

openFile :: FilePath -> IOMode -> TimeMitM IO (Handle)
openFile f mode = mkMitigated Nothing (mkQuant 1000) $ do
  h <- SIO.openFile f mode
  SIO.hSetBuffering h NoBuffering
  return h

hPut :: Handle -> BS.ByteString -> TimeMitM IO ()
hPut mH bs = mitigateWrite mH $ \h -> BS.hPut h bs

hPutStrLn :: Handle -> BS.ByteString -> TimeMitM IO ()
hPutStrLn mH bs = mitigateWrite mH $ \h -> BS.hPutStrLn h bs

hGetLine :: Handle -> TimeMitM IO BS.ByteString
hGetLine = lift . BS.hGetLine . mitVal

hGetContents :: Handle -> TimeMitM IO BS.ByteString
hGetContents = lift . BS.hGetContents . mitVal

hGet :: Handle -> Int -> TimeMitM IO BS.ByteString
hGet mH = lift . (BS.hGet . mitVal $ mH)

hClose :: Handle -> TimeMitM IO ()
hClose = lift . SIO.hClose . mitVal

hIsEOF :: Handle -> TimeMitM IO Bool
hIsEOF = lift . SIO.hIsEOF . mitVal

hIsOpen :: Handle -> TimeMitM IO Bool
hIsOpen = lift . SIO.hIsOpen . mitVal

hIsClosed :: Handle -> TimeMitM IO Bool
hIsClosed = lift . SIO.hIsClosed . mitVal

hIsReadable :: Handle -> TimeMitM IO Bool
hIsReadable = lift . SIO.hIsReadable . mitVal

hIsWriteable :: Handle -> TimeMitM IO Bool
hIsWriteable = lift . SIO.hIsWritable . mitVal

{-
hFlush :: Handle -> TimeMitM IO ()
hFlush = lift . SIO.hFlush . mitVal

test = evalMitM $ do
  h <- openFile "/tmp/woz" ReadWriteMode
  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world") >> hFlush h
  lift $ putStrLn "about to miss" >> threadDelay 900 >> putStrLn "DONE!"
  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world") >> hFlush h
--  hPut h (BS.pack "hello world") >> hFlush h
--  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world") >> hFlush h
--  lift $ putStrLn "starting read"
--  sequence_ $ replicate 15 $ hGet h 1 >>= \b -> lift $ BS.putStrLn b
  hClose h



-}
