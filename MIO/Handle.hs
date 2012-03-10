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

-- | Time-mitigated handle.
type Handle = TimeMitigated SIO.Handle

-- | Given a file path, operation mode, and initial quantum open file.
-- Note that the quantum is only used to mitigate writes (with 'hPut'
-- and 'hPutStrLn') and thus is ignored if the file mode is 'ReadMode'.
openFile :: FilePath -> IOMode -> Integer -> TimeMitM IO Handle
openFile f mode q = mkMitigated Nothing (mkQuant q) $ do
  h <- SIO.openFile f mode
  SIO.hSetBuffering h NoBuffering
  return h

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> BS.ByteString -> TimeMitM IO ()
hPut mH bs = mitigateWrite mH $ \h -> BS.hPut h bs

-- | Outputs a 'ByteString' to the specified 'Handle', appending a
-- newline byte.
hPutStrLn :: Handle -> BS.ByteString -> TimeMitM IO ()
hPutStrLn mH bs = mitigateWrite mH $ \h -> BS.hPutStrLn h bs

-- | Read a line from handle.
hGetLine :: Handle -> TimeMitM IO BS.ByteString
hGetLine = lift . BS.hGetLine . mitVal

-- | Read entier handle contents, strictly into a 'Bytestring'.
hGetContents :: Handle -> TimeMitM IO BS.ByteString
hGetContents = lift . BS.hGetContents . mitVal

-- | Read a 'ByteString' directly from specified 'Handle'.
hGet :: Handle -> Int -> TimeMitM IO BS.ByteString
hGet mH = lift . (BS.hGet . mitVal $ mH)

-- | Close a 'Handle'.
hClose :: Handle -> TimeMitM IO ()
hClose mH = wait mH $ \h -> SIO.hClose h

-- | Check if @EOF@ has been reached.
hIsEOF :: Handle -> TimeMitM IO Bool
hIsEOF = lift . SIO.hIsEOF . mitVal

-- | Check if 'Handle' is open.
hIsOpen :: Handle -> TimeMitM IO Bool
hIsOpen = lift . SIO.hIsOpen . mitVal

-- | Check if 'Handle' is closed.
hIsClosed :: Handle -> TimeMitM IO Bool
hIsClosed = lift . SIO.hIsClosed . mitVal

-- | Check if 'Handle' is readable.
hIsReadable :: Handle -> TimeMitM IO Bool
hIsReadable = lift . SIO.hIsReadable . mitVal

-- | Check if 'Handle' is writeable.
hIsWriteable :: Handle -> TimeMitM IO Bool
hIsWriteable = lift . SIO.hIsWritable . mitVal
