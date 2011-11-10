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
import System.Posix.Unistd
import System.IO (IO, IOMode(..), BufferMode(..))
import qualified System.IO as SIO
import qualified Data.ByteString.Char8 as BS

type Handle = TimeMitigated SIO.Handle

openFile :: FilePath -> IOMode -> MIOTime IO (Handle)
openFile f mode = mitigateC (quant $ milliToNano 1000) $ do
  h <- SIO.openFile f mode
  SIO.hSetBuffering h NoBuffering
  return h

hPut :: Handle -> BS.ByteString -> MIOTime IO ()
hPut mH bs = mitigate mH $ \h -> liftMIO $ BS.hPut h bs

hPutStrLn :: Handle -> BS.ByteString -> MIOTime IO ()
hPutStrLn mH bs = mitigate mH $ \h -> liftMIO $ BS.hPutStrLn h bs

hGetLine :: Handle -> MIOTime IO BS.ByteString
hGetLine = liftMIO . (BS.hGetLine . mitVal)

hGetContents :: Handle -> MIOTime IO BS.ByteString
hGetContents = liftMIO . (BS.hGetContents . mitVal)

hGet :: Handle -> Int -> MIOTime IO BS.ByteString
hGet mH = liftMIO . (BS.hGet . mitVal $ mH)

hClose :: Handle -> MIOTime IO ()
hClose = liftMIO . (SIO.hClose . mitVal)

hIsEOF :: Handle -> MIOTime IO Bool
hIsEOF = liftMIO . (SIO.hIsEOF . mitVal)

hIsOpen :: Handle -> MIOTime IO Bool
hIsOpen = liftMIO . (SIO.hIsOpen . mitVal)

hIsClosed :: Handle -> MIOTime IO Bool
hIsClosed = liftMIO . (SIO.hIsClosed . mitVal)

hIsReadable :: Handle -> MIOTime IO Bool
hIsReadable = liftMIO . (SIO.hIsReadable . mitVal)

hIsWriteable :: Handle -> MIOTime IO Bool
hIsWriteable = liftMIO . (SIO.hIsWritable . mitVal)


test = evalMIO $ do
  h <- openFile "/tmp/woo" ReadWriteMode
  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world") -- >> hFlush h
  liftMIO $ putStrLn "about to miss" >> sleep 2 >> putStrLn "DONE!"
  hPut h (BS.pack "hello world") -- >> hFlush h
  sequence_ $ replicate 5 $ hPut h (BS.pack "hello world") -- >> hFlush h
  liftMIO $ putStrLn "starting read"
  sequence_ $ replicate 15 $ hGet h 1 >>= \b -> liftMIO $ BS.putStrLn b
  hClose h



