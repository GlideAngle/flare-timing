module Flight.Fsdb
    ( readCleanFsdb, writeCleanFsdb
    , readTrimFsdb, writeTrimFsdb
    ) where

import Prelude hiding (readFile, writeFile)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Flight.Comp (CleanFsdbFile(..), TrimFsdbFile(..), FsdbXml(..))

readFsdbXml :: FilePath -> IO FsdbXml
readFsdbXml path = FsdbXml . T.unpack . T.decodeUtf8 <$> BS.readFile path

writeFsdbXml :: FilePath -> FsdbXml -> IO ()
writeFsdbXml path (FsdbXml contents) = do

    -- SEE: https://stackoverflow.com/questions/58682357/how-to-create-a-file-and-its-parent-directories-in-haskell
    createDirectoryIfMissing True $ takeDirectory path
    BS.writeFile path (T.encodeUtf8 $ T.pack contents)

readCleanFsdb :: CleanFsdbFile -> IO FsdbXml
readCleanFsdb (CleanFsdbFile path) = readFsdbXml path

writeCleanFsdb :: CleanFsdbFile -> FsdbXml -> IO ()
writeCleanFsdb (CleanFsdbFile path) = writeFsdbXml path

readTrimFsdb :: TrimFsdbFile -> IO FsdbXml
readTrimFsdb (TrimFsdbFile path) = readFsdbXml path

writeTrimFsdb :: TrimFsdbFile -> FsdbXml -> IO ()
writeTrimFsdb (TrimFsdbFile path) = writeFsdbXml path
