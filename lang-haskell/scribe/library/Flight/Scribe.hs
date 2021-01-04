module Flight.Scribe
    ( readAltRoute, writeAltRoute
    , readCleanFsdb, writeCleanFsdb
    , readTrimFsdb, writeTrimFsdb
    , module Flight.CompInput
    , module Flight.TaskLength
    , module Flight.FlyTime
    , module Flight.CrossZone
    , module Flight.TagZone
    , module Flight.PegFrame
    , module Flight.LeadArea
    , module Flight.LeadArea.AreaStep
    , module Flight.Mask.Arrival
    , module Flight.Mask.Bonus
    , module Flight.Mask.Effort
    , module Flight.Mask.Reach
    , module Flight.Mask.Speed
    , module Flight.Mask.Lead
    , module Flight.UnpackTrack
    , module Flight.AlignTime
    , module Flight.DiscardFurther
    , module Flight.LandOut
    , module Flight.FarOut
    , module Flight.GapPoint
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Yaml.Pretty as Y

import Flight.Route (GeoLines)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CleanFsdbFile(..)
    , TrimFsdbFile(..)
    , AltRouteFile(..)
    , FsdbXml(..)
    )
import Flight.CompInput
import Flight.TaskLength
import Flight.FlyTime
import Flight.CrossZone
import Flight.TagZone
import Flight.UnpackTrack
import Flight.PegFrame
import Flight.AlignTime
import Flight.DiscardFurther
import Flight.LeadArea
import Flight.LeadArea.AreaStep
import Flight.Mask.Arrival
import Flight.Mask.Speed
import Flight.Mask.Bonus
import Flight.Mask.Effort
import Flight.Mask.Lead
import Flight.Mask.Reach
import Flight.LandOut
import Flight.FarOut
import Flight.GapPoint

readFsdbXml :: FilePath -> IO FsdbXml
readFsdbXml path = FsdbXml . T.unpack . T.decodeUtf8 <$> BS.readFile path

writeFsdbXml :: FilePath -> FsdbXml -> IO ()
writeFsdbXml path (FsdbXml contents) =
    BS.writeFile path (T.encodeUtf8 $ T.pack contents)

readCleanFsdb :: CleanFsdbFile -> IO FsdbXml
readCleanFsdb (CleanFsdbFile path) = readFsdbXml path

writeCleanFsdb :: CleanFsdbFile -> FsdbXml -> IO ()
writeCleanFsdb (CleanFsdbFile path) = writeFsdbXml path

readTrimFsdb :: TrimFsdbFile -> IO FsdbXml
readTrimFsdb (TrimFsdbFile path) = readFsdbXml path

writeTrimFsdb :: TrimFsdbFile -> FsdbXml -> IO ()
writeTrimFsdb (TrimFsdbFile path) = writeFsdbXml path

readAltRoute :: (MonadThrow m, MonadIO m) => AltRouteFile -> m [GeoLines]
readAltRoute (AltRouteFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltRoute :: AltRouteFile -> [GeoLines] -> IO ()
writeAltRoute (AltRouteFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml
