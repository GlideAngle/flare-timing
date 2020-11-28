module Flight.Scribe
    ( readNormArrival, writeNormArrival
    , readNormLandout, writeNormLandout
    , readNormRoute, writeNormRoute
    , readNormScore, writeNormScore
    , readComp, writeComp
    , readRoute, writeRoute
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

import Flight.Route (TaskTrack(..), cmpFields)
import Flight.Track.Cross (Crossing)
import Flight.Track.Tag (Tagging(..))
import Flight.Track.Stop (Framing(..))
import Flight.Track.Mask
    (MaskingArrival, MaskingEffort, MaskingReach, MaskingSpeed, MaskingLead)
import Flight.Track.Lead (DiscardingLead)
import Flight.Track.Land (Landing)
import Flight.Track.Point (Pointing, NormPointing)
import Flight.Route (GeoLines)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CleanFsdbFile(..)
    , TrimFsdbFile(..)
    , NormArrivalFile(..)
    , NormLandoutFile(..)
    , NormRouteFile(..)
    , NormScoreFile(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
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
    , CompSettings(..)
    , FsdbXml(..)
    )
import Flight.UnpackTrack
import Flight.AlignTime
import Flight.DiscardFurther
import Flight.AreaStep

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

readComp
    :: (MonadThrow m, MonadIO m)
    => CompInputFile
    -> m (CompSettings k)
readComp (CompInputFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeComp :: CompInputFile -> CompSettings k -> IO ()
writeComp (CompInputFile path) compInput = do
    let cfg = Y.setConfCompare (fieldOrder compInput) Y.defConfig
    let yaml = Y.encodePretty cfg compInput
    BS.writeFile path yaml

readNormScore
    :: (MonadThrow m, MonadIO m)
    => NormScoreFile
    -> m NormPointing
readNormScore (NormScoreFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeNormScore :: NormScoreFile -> NormPointing -> IO ()
writeNormScore (NormScoreFile path) pointing = do
    let cfg = Y.setConfCompare (fieldOrder pointing) Y.defConfig
    let yaml = Y.encodePretty cfg pointing
    BS.writeFile path yaml

readNormArrival
    :: (MonadThrow m, MonadIO m)
    => NormArrivalFile
    -> m MaskingArrival
readNormArrival (NormArrivalFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeNormArrival :: NormArrivalFile -> MaskingArrival -> IO ()
writeNormArrival (NormArrivalFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readNormLandout
    :: (MonadThrow m, MonadIO m)
    => NormLandoutFile
    -> m Landing
readNormLandout (NormLandoutFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeNormLandout :: NormLandoutFile -> Landing -> IO ()
writeNormLandout (NormLandoutFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readNormRoute
    :: (MonadThrow m, MonadIO m)
    => NormRouteFile
    -> m [GeoLines]
readNormRoute (NormRouteFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeNormRoute :: NormRouteFile -> [GeoLines] -> IO ()
writeNormRoute (NormRouteFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readRoute
    :: (MonadThrow m, MonadIO m)
    => TaskLengthFile
    -> m [Maybe TaskTrack]
readRoute (TaskLengthFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeRoute :: TaskLengthFile -> [Maybe TaskTrack] -> IO ()
writeRoute (TaskLengthFile lenPath) route =
    BS.writeFile lenPath yaml
    where
        cfg = Y.setConfCompare cmpFields Y.defConfig
        yaml = Y.encodePretty cfg route

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
