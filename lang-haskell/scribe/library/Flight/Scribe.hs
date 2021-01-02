module Flight.Scribe
    ( readAltArrival, writeAltArrival
    , readAltLandout, writeAltLandout
    , readAltRoute, writeAltRoute
    , readAltScore, writeAltScore
    , readFlying , writeFlying
    , readCrossing , writeCrossing
    , readTagging, writeTagging
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
    , module Flight.UnpackTrack
    , module Flight.AlignTime
    , module Flight.DiscardFurther
    , module Flight.AreaStep
    , module Flight.TaskLength
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

import Flight.Track.Cross (Flying, Crossing)
import Flight.Track.Tag (Tagging(..))
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
    , FlyTimeFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
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
import Flight.UnpackTrack
import Flight.AlignTime
import Flight.DiscardFurther
import Flight.AreaStep
import Flight.TaskLength

readFsdbXml :: FilePath -> IO FsdbXml
readFsdbXml path =
    FsdbXml . T.unpack . T.decodeUtf8 <$> BS.readFile path

writeFsdbXml :: FilePath -> FsdbXml -> IO ()
writeFsdbXml path (FsdbXml contents) =
    BS.writeFile path (T.encodeUtf8 $ T.pack contents)

readCleanFsdb :: CleanFsdbFile -> IO FsdbXml
readCleanFsdb (CleanFsdbFile path) = readFsdbXml path

writeCleanFsdb :: CleanFsdbFile -> FsdbXml -> IO ()
writeCleanFsdb (CleanFsdbFile path) fsdbXml  =
    writeFsdbXml path fsdbXml

readTrimFsdb :: TrimFsdbFile -> IO FsdbXml
readTrimFsdb (TrimFsdbFile path) = readFsdbXml path

writeTrimFsdb :: TrimFsdbFile -> FsdbXml -> IO ()
writeTrimFsdb (TrimFsdbFile path) fsdbXml =
    writeFsdbXml path fsdbXml

readAltScore
    :: (MonadThrow m, MonadIO m)
    => AltScoreFile
    -> m AltPointing
readAltScore (AltScoreFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeAltScore :: AltScoreFile -> AltPointing -> IO ()
writeAltScore (AltScoreFile path) pointing = do
    let cfg = Y.setConfCompare (fieldOrder pointing) Y.defConfig
    let yaml = Y.encodePretty cfg pointing
    BS.writeFile path yaml

readAltArrival
    :: (MonadThrow m, MonadIO m)
    => AltArrivalFile
    -> m MaskingArrival
readAltArrival (AltArrivalFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeAltArrival :: AltArrivalFile -> MaskingArrival -> IO ()
writeAltArrival (AltArrivalFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readAltLandout
    :: (MonadThrow m, MonadIO m)
    => AltLandoutFile
    -> m Landing
readAltLandout (AltLandoutFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeAltLandout :: AltLandoutFile -> Landing -> IO ()
writeAltLandout (AltLandoutFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readAltRoute
    :: (MonadThrow m, MonadIO m)
    => AltRouteFile
    -> m [GeoLines]
readAltRoute (AltRouteFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeAltRoute :: AltRouteFile -> [GeoLines] -> IO ()
writeAltRoute (AltRouteFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readFlying
    :: (MonadThrow m, MonadIO m)
    => FlyTimeFile
    -> m Flying
readFlying (FlyTimeFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeFlying :: FlyTimeFile -> Flying -> IO ()
writeFlying (FlyTimeFile path) flyTime = do
    let cfg = Y.setConfCompare (fieldOrder flyTime) Y.defConfig
    let yaml = Y.encodePretty cfg flyTime
    BS.writeFile path yaml

readCrossing
    :: (MonadThrow m, MonadIO m)
    => CrossZoneFile
    -> m Crossing
readCrossing (CrossZoneFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeCrossing :: CrossZoneFile -> Crossing -> IO ()
writeCrossing (CrossZoneFile path) crossZone = do
    let cfg = Y.setConfCompare (fieldOrder crossZone) Y.defConfig
    let yaml = Y.encodePretty cfg crossZone
    BS.writeFile path yaml

readTagging
    :: (MonadThrow m, MonadIO m)
    => TagZoneFile
    -> m Tagging
readTagging (TagZoneFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeTagging :: TagZoneFile -> Tagging -> IO ()
writeTagging (TagZoneFile path) tagZone = do
    let cfg = Y.setConfCompare (fieldOrder tagZone) Y.defConfig
    let yaml = Y.encodePretty cfg tagZone
    BS.writeFile path yaml

readFraming
    :: (MonadThrow m, MonadIO m)
    => PegFrameFile
    -> m Framing
readFraming (PegFrameFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeFraming :: PegFrameFile -> Framing -> IO ()
writeFraming (PegFrameFile path) stopTask = do
    let cfg = Y.setConfCompare (fieldOrder stopTask) Y.defConfig
    let yaml = Y.encodePretty cfg stopTask
    BS.writeFile path yaml

readDiscardingLead
    :: (MonadIO m, FromJSON (DiscardingLead q))
    => LeadAreaFile
    -> m (DiscardingLead q)
readDiscardingLead (LeadAreaFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingArrival
    :: MonadIO m
    => MaskArrivalFile
    -> m MaskingArrival
readMaskingArrival (MaskArrivalFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingEffort
    :: MonadIO m
    => MaskEffortFile
    -> m MaskingEffort
readMaskingEffort (MaskEffortFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingLead
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v), MonadIO m)
    => MaskLeadFile
    -> m (MaskingLead u v)
readMaskingLead (MaskLeadFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingReach
    :: MonadIO m
    => MaskReachFile
    -> m MaskingReach
readMaskingReach (MaskReachFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingSpeed
    :: MonadIO m
    => MaskSpeedFile
    -> m MaskingSpeed
readMaskingSpeed (MaskSpeedFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readBonusReach
    :: MonadIO m
    => BonusReachFile
    -> m MaskingReach
readBonusReach (BonusReachFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

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

readLanding
    :: (MonadThrow m, MonadIO m)
    => LandOutFile
    -> m Landing
readLanding (LandOutFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeLanding :: LandOutFile -> Landing -> IO ()
writeLanding (LandOutFile path) landout = do
    let cfg = Y.setConfCompare (fieldOrder landout) Y.defConfig
    let yaml = Y.encodePretty cfg landout
    BS.writeFile path yaml

readFaring
    :: (MonadThrow m, MonadIO m)
    => FarOutFile
    -> m Landing
readFaring (FarOutFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeFaring :: FarOutFile -> Landing -> IO ()
writeFaring (FarOutFile path) landout = do
    let cfg = Y.setConfCompare (fieldOrder landout) Y.defConfig
    let yaml = Y.encodePretty cfg landout
    BS.writeFile path yaml

readPointing
    :: (MonadThrow m, MonadIO m)
    => GapPointFile
    -> m Pointing
readPointing (GapPointFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writePointing :: GapPointFile -> Pointing -> IO ()
writePointing (GapPointFile path) gapPoint = do
    let cfg = Y.setConfCompare (fieldOrder gapPoint) Y.defConfig
    let yaml = Y.encodePretty cfg gapPoint
    BS.writeFile path yaml
