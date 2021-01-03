module Flight.Scribe
    ( readAltArrival, writeAltArrival
    , readAltLandout, writeAltLandout
    , readAltRoute, writeAltRoute
    , readAltScore, writeAltScore
    , readFraming, writeFraming
    , readMaskingArrival, writeMaskingArrival
    , readMaskingEffort, writeMaskingEffort
    , readDiscardingLead, writeDiscardingLead
    , readMaskingLead, writeMaskingLead
    , readMaskingReach, writeMaskingReach
    , readMaskingSpeed, writeMaskingSpeed
    , readBonusReach, writeBonusReach
    , readLanding, writeLanding
    , readFaring, writeFaring
    , readPointing, writePointing
    , readCleanFsdb, writeCleanFsdb
    , readTrimFsdb, writeTrimFsdb
    , module Flight.CompInput
    , module Flight.TaskLength
    , module Flight.FlyTime
    , module Flight.CrossZone
    , module Flight.TagZone
    , module Flight.UnpackTrack
    , module Flight.AlignTime
    , module Flight.DiscardFurther
    , module Flight.AreaStep
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Yaml (decodeThrow)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Yaml.Pretty as Y
import Data.UnitsOfMeasure (KnownUnit, Unpack)

import Flight.Track.Stop (Framing(..))
import Flight.Track.Mask
    (MaskingArrival, MaskingEffort, MaskingReach, MaskingSpeed, MaskingLead)
import Flight.Track.Lead (DiscardingLead)
import Flight.Track.Land (Landing)
import Flight.Track.Point (Pointing, AltPointing)
import Flight.Route (GeoLines)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CleanFsdbFile(..)
    , TrimFsdbFile(..)
    , AltArrivalFile(..)
    , AltLandoutFile(..)
    , AltRouteFile(..)
    , AltScoreFile(..)
    , PegFrameFile(..)
    , LeadAreaFile(..)
    , MaskArrivalFile(..)
    , MaskEffortFile(..)
    , MaskLeadFile(..)
    , MaskReachFile(..)
    , MaskSpeedFile(..)
    , BonusReachFile(..)
    , LandOutFile(..)
    , FarOutFile(..)
    , GapPointFile(..)
    , FsdbXml(..)
    )
import Flight.CompInput
import Flight.TaskLength
import Flight.FlyTime
import Flight.CrossZone
import Flight.TagZone
import Flight.UnpackTrack
import Flight.AlignTime
import Flight.DiscardFurther
import Flight.AreaStep

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

readAltScore :: (MonadThrow m, MonadIO m) => AltScoreFile -> m AltPointing
readAltScore (AltScoreFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltScore :: AltScoreFile -> AltPointing -> IO ()
writeAltScore (AltScoreFile path) pointing = do
    let cfg = Y.setConfCompare (fieldOrder pointing) Y.defConfig
    let yaml = Y.encodePretty cfg pointing
    BS.writeFile path yaml

readAltArrival :: (MonadThrow m, MonadIO m) => AltArrivalFile -> m MaskingArrival
readAltArrival (AltArrivalFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltArrival :: AltArrivalFile -> MaskingArrival -> IO ()
writeAltArrival (AltArrivalFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readAltLandout :: (MonadThrow m, MonadIO m) => AltLandoutFile -> m Landing
readAltLandout (AltLandoutFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltLandout :: AltLandoutFile -> Landing -> IO ()
writeAltLandout (AltLandoutFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readAltRoute :: (MonadThrow m, MonadIO m) => AltRouteFile -> m [GeoLines]
readAltRoute (AltRouteFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltRoute :: AltRouteFile -> [GeoLines] -> IO ()
writeAltRoute (AltRouteFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readFraming :: (MonadThrow m, MonadIO m) => PegFrameFile -> m Framing
readFraming (PegFrameFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeFraming :: PegFrameFile -> Framing -> IO ()
writeFraming (PegFrameFile path) stopTask = do
    let cfg = Y.setConfCompare (fieldOrder stopTask) Y.defConfig
    let yaml = Y.encodePretty cfg stopTask
    BS.writeFile path yaml

readDiscardingLead
    :: (MonadIO m, FromJSON (DiscardingLead q))
    => LeadAreaFile
    -> m (DiscardingLead q)
readDiscardingLead (LeadAreaFile path) = liftIO $ BS.readFile path >>= decodeThrow

readMaskingArrival :: MonadIO m => MaskArrivalFile -> m MaskingArrival
readMaskingArrival (MaskArrivalFile path) = liftIO $ BS.readFile path >>= decodeThrow

readMaskingEffort :: MonadIO m => MaskEffortFile -> m MaskingEffort
readMaskingEffort (MaskEffortFile path) = liftIO $ BS.readFile path >>= decodeThrow

readMaskingLead
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v), MonadIO m)
    => MaskLeadFile
    -> m (MaskingLead u v)
readMaskingLead (MaskLeadFile path) = liftIO $ BS.readFile path >>= decodeThrow

readMaskingReach :: MonadIO m => MaskReachFile -> m MaskingReach
readMaskingReach (MaskReachFile path) = liftIO $ BS.readFile path >>= decodeThrow

readMaskingSpeed :: MonadIO m => MaskSpeedFile -> m MaskingSpeed
readMaskingSpeed (MaskSpeedFile path) = liftIO $ BS.readFile path >>= decodeThrow

readBonusReach :: MonadIO m => BonusReachFile -> m MaskingReach
readBonusReach (BonusReachFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeDiscardingLead
    :: (ToJSON (DiscardingLead q))
    => LeadAreaFile
    -> DiscardingLead q
    -> IO ()
writeDiscardingLead (LeadAreaFile path) discardingLead = do
    let cfg = Y.setConfCompare (fieldOrder discardingLead) Y.defConfig
    let yaml = Y.encodePretty cfg discardingLead
    BS.writeFile path yaml

writeMaskingArrival :: MaskArrivalFile -> MaskingArrival -> IO ()
writeMaskingArrival (MaskArrivalFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml

writeMaskingEffort :: MaskEffortFile -> MaskingEffort -> IO ()
writeMaskingEffort (MaskEffortFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml

writeMaskingLead
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v))
    => MaskLeadFile
    -> MaskingLead u v
    -> IO ()
writeMaskingLead (MaskLeadFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml

writeMaskingReach :: MaskReachFile -> MaskingReach -> IO ()
writeMaskingReach (MaskReachFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml

writeBonusReach :: BonusReachFile -> MaskingReach -> IO ()
writeBonusReach (BonusReachFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml

writeMaskingSpeed :: MaskSpeedFile -> MaskingSpeed -> IO ()
writeMaskingSpeed (MaskSpeedFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml

readLanding :: (MonadThrow m, MonadIO m) => LandOutFile -> m Landing
readLanding (LandOutFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeLanding :: LandOutFile -> Landing -> IO ()
writeLanding (LandOutFile path) landout = do
    let cfg = Y.setConfCompare (fieldOrder landout) Y.defConfig
    let yaml = Y.encodePretty cfg landout
    BS.writeFile path yaml

readFaring :: (MonadThrow m, MonadIO m) => FarOutFile -> m Landing
readFaring (FarOutFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeFaring :: FarOutFile -> Landing -> IO ()
writeFaring (FarOutFile path) landout = do
    let cfg = Y.setConfCompare (fieldOrder landout) Y.defConfig
    let yaml = Y.encodePretty cfg landout
    BS.writeFile path yaml

readPointing :: (MonadThrow m, MonadIO m) => GapPointFile -> m Pointing
readPointing (GapPointFile path) = liftIO $ BS.readFile path >>= decodeThrow

writePointing :: GapPointFile -> Pointing -> IO ()
writePointing (GapPointFile path) gapPoint = do
    let cfg = Y.setConfCompare (fieldOrder gapPoint) Y.defConfig
    let yaml = Y.encodePretty cfg gapPoint
    BS.writeFile path yaml
