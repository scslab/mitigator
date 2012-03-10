-- | Simple time-mitigated interface  IO with handles
-- Note that reads are not mitigated as on most real systems you can
-- disable @atime@.
module MIO.Handle ( MIO
                  , FilePath
                  , Handle
                  , openFile
                  , IOMode(..)
                  , hClose
                  , hPut
                  , hPutStrLn
                  , hGetContents
                  , hGet
                  , hGetNonBlocking 
                  , hWaitForInput 
                  , hIsEOF
                  , hIsOpen
                  , hIsClosed
                  , hIsReadable
                  , hIsWriteable
                  , hSetBinaryMode
                  -- * Sockets
                  , socketToHandle
                  ) where
import Mitigator
import Mitigator.Time
import System.IO (BufferMode(..))
import qualified System.IO as IO
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad.Trans
import Network.Socket ( Socket )
import qualified Network.Socket as Net

-- | Time mitigated IO monad.
type MIO = TimeMitM IO 

-- | Time-mitigated handle.
type Handle = TimeMitigated IO.Handle

-- | File read/write mode.
data IOMode = ReadMode               -- ^ Read-only
            | WriteMode Integer      -- ^ Mitigated write-only
            | AppendMode Integer     -- ^ Mitigated write-only
            | ReadWriteMode Integer  -- ^ Mitigated read-write
            deriving (Eq, Show)

-- | Given a file path, and operation modea open file.
-- Note that the quantum is only used to mitigate writes (with 'hPut'
-- and 'hPutStrLn') and thus is supplied with the 'IOMode'.
-- By default the handle is not buffered.
openFile :: FilePath -> IOMode -> MIO Handle
openFile f mMode = mkMitigated Nothing (mkQuant q) $ do
  h <- IO.openFile f mode
  IO.hSetBuffering h NoBuffering
  return h
    where (q,mode) = modeToIOMode mMode

--  | Convert 'IOMode' to pair of quantum length and actual mode.
modeToIOMode :: IOMode -> (Integer, IO.IOMode)
modeToIOMode mMode = 
    case mMode of
      WriteMode x     -> (x, IO.WriteMode)
      AppendMode x    -> (x, IO.AppendMode)
      ReadMode        -> (0, IO.ReadMode)
      ReadWriteMode x -> (x, IO.ReadWriteMode)

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> BS.ByteString -> MIO ()
hPut mH bs = mitigateWrite mH $ \h -> BS.hPut h bs

-- | Outputs a 'ByteString' to the specified 'Handle', appending a
-- newline byte.
hPutStrLn :: Handle -> BS.ByteString -> MIO ()
hPutStrLn mH bs = mitigateWrite mH $ \h -> BS.hPutStrLn h bs

-- | Read entier handle contents, strictly into a 'Bytestring'.
hGetContents :: Handle -> MIO BS.ByteString
hGetContents = lift . BS.hGetContents . mitVal

-- | Read a 'ByteString' directly from specified 'Handle'.
hGet :: Handle -> Int -> MIO BS.ByteString
hGet mH = lift . (BS.hGet . mitVal $ mH)

-- | Read a 'ByteString' directly from specified 'Handle', without
-- blocking.
hGetNonBlocking :: Handle -> Int -> MIO BS.ByteString
hGetNonBlocking mH = lift . (BS.hGetNonBlocking . mitVal $ mH)

-- | WAitfor input until its available on handle.
hWaitForInput :: Handle -> Int -> MIO Bool
hWaitForInput mH = lift . (IO.hWaitForInput . mitVal $ mH)

-- | Close a 'Handle'.
hClose :: Handle -> MIO ()
hClose mH = wait mH $ \h -> IO.hClose h

-- | Check if @EOF@ has been reached.
hIsEOF :: Handle -> MIO Bool
hIsEOF = lift . IO.hIsEOF . mitVal

-- | Check if 'Handle' is open.
hIsOpen :: Handle -> MIO Bool
hIsOpen = lift . IO.hIsOpen . mitVal

-- | Check if 'Handle' is closed.
hIsClosed :: Handle -> MIO Bool
hIsClosed = lift . IO.hIsClosed . mitVal

-- | Check if 'Handle' is readable.
hIsReadable :: Handle -> MIO Bool
hIsReadable = lift . IO.hIsReadable . mitVal

-- | Check if 'Handle' is writeable.
hIsWriteable :: Handle -> MIO Bool
hIsWriteable = lift . IO.hIsWritable . mitVal

-- | Select binary mode.
hSetBinaryMode :: Handle -> Bool -> MIO ()
hSetBinaryMode mH = lift . (IO.hSetBinaryMode . mitVal $ mH)


-- | Convert a socket to a handle, disabling the Nagle algorithm.
socketToHandle :: Socket -> IOMode -> MIO Handle
socketToHandle sock mMode = mkMitigated Nothing (mkQuant q) $ do
  Net.setSocketOption sock Net.NoDelay 1
  Net.socketToHandle sock mode
    where (q, mode) = modeToIOMode mMode


