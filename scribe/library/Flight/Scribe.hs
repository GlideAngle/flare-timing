module Flight.Scribe
    ( readNormEffort, writeNormEffort
    , readNormRoute, writeNormRoute
    , readNormScore, writeNormScore
    , readComp, writeComp
    , readRoute, writeRoute
    , readCrossing , writeCrossing
    , readTagging, writeTagging
    , readFraming, writeFraming
    , readMaskingArrival, writeMaskingArrival
    , readMaskingEffort, writeMaskingEffort
    , readMaskingLead, writeMaskingLead
    , readMaskingReach, writeMaskingReach
    , readMaskingSpeed, writeMaskingSpeed
    , readBonusReach, writeBonusReach
    , readLanding, writeLanding
    , readPointing, writePointing
    , readCleanFsdb, writeCleanFsdb
    , readTrimFsdb, writeTrimFsdb
    , module Flight.UnpackTrack
    , module Flight.AlignTime
    , module Flight.DiscardFurther
    ) where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Route (TaskTrack(..), cmpFields)
import Flight.Track.Cross (Crossing)
import Flight.Track.Tag (Tagging(..))
import Flight.Track.Stop (Framing(..))
import Flight.Track.Mask
    (MaskingArrival, MaskingEffort, MaskingLead, MaskingReach, MaskingSpeed)
import Flight.Track.Land (Landing)
import Flight.Track.Point (Pointing, NormPointing)
import Flight.Route (GeoLines)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CleanFsdbFile(..)
    , TrimFsdbFile(..)
    , NormEffortFile(..)
    , NormRouteFile(..)
    , NormScoreFile(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , PegFrameFile(..)
    , MaskArrivalFile(..)
    , MaskEffortFile(..)
    , MaskLeadFile(..)
    , MaskReachFile(..)
    , MaskSpeedFile(..)
    , BonusReachFile(..)
    , LandOutFile(..)
    , GapPointFile(..)
    , CompSettings(..)
    , FsdbXml(..)
    )
import Flight.UnpackTrack
import Flight.AlignTime
import Flight.DiscardFurther

readCleanFsdb :: CleanFsdbFile -> IO FsdbXml
readCleanFsdb (CleanFsdbFile path) =
    FsdbXml <$> readFile path

writeCleanFsdb :: CleanFsdbFile -> FsdbXml -> IO ()
writeCleanFsdb (CleanFsdbFile path) (FsdbXml contents) =
    writeFile path contents

readTrimFsdb :: TrimFsdbFile -> IO FsdbXml
readTrimFsdb (TrimFsdbFile path) =
    FsdbXml <$> readFile path

writeTrimFsdb :: TrimFsdbFile -> FsdbXml -> IO ()
writeTrimFsdb (TrimFsdbFile path) (FsdbXml contents) =
    writeFile path contents

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

readNormEffort
    :: (MonadThrow m, MonadIO m)
    => NormEffortFile
    -> m Landing
readNormEffort (NormEffortFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeNormEffort :: NormEffortFile -> Landing -> IO ()
writeNormEffort (NormEffortFile path) track = do
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

readMaskingArrival
    :: (MonadThrow m, MonadIO m)
    => MaskArrivalFile
    -> m MaskingArrival
readMaskingArrival (MaskArrivalFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingEffort
    :: (MonadThrow m, MonadIO m)
    => MaskEffortFile
    -> m MaskingEffort
readMaskingEffort (MaskEffortFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingLead
    :: (MonadThrow m, MonadIO m)
    => MaskLeadFile
    -> m MaskingLead
readMaskingLead (MaskLeadFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingReach
    :: (MonadThrow m, MonadIO m)
    => MaskReachFile
    -> m MaskingReach
readMaskingReach (MaskReachFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readMaskingSpeed
    :: (MonadThrow m, MonadIO m)
    => MaskSpeedFile
    -> m MaskingSpeed
readMaskingSpeed (MaskSpeedFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

readBonusReach
    :: (MonadThrow m, MonadIO m)
    => BonusReachFile
    -> m MaskingReach
readBonusReach (BonusReachFile path) =
    liftIO $ BS.readFile path >>= decodeThrow

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

writeMaskingLead :: MaskLeadFile -> MaskingLead -> IO ()
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
