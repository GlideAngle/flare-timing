module Flight.Path.Tx
    ( trimFsdbToNormArrival
    , trimFsdbToNormLandout
    , trimFsdbToNormRoute
    , trimFsdbToNormScore
    , compToNormArrival
    , compToNormLandout
    , compToNormRoute
    , compToNormScore
    , fsdbToCleanFsdb
    , cleanFsdbToTrimFsdb
    , trimFsdbToComp
    , compToTaskLength
    , compToCross
    , compToLeadArea
    , compToMaskArrival
    , compToMaskEffort
    , compToMaskLead
    , compToMaskReach
    , compToMaskSpeed
    , compToBonusReach
    , compToLand
    , compToFar
    , compToPoint
    , crossToTag
    , tagToPeg
    , compFileToCompDir
    , unpackTrackDir
    , alignTimeDir
    , discardFurtherDir
    , pegThenDiscardDir
    , areaStepDir
    , unpackTrackPath
    , alignTimePath
    , discardFurtherPath
    , pegThenDiscardPath
    , areaStepPath
    ) where

import Text.Printf (printf)
import System.FilePath (FilePath, (</>), (<.>), takeDirectory)
import "flight-gap-allot" Flight.Score (PilotId(..), PilotName(..), Pilot(..))
import Flight.Path.Types

dotDir :: DotFolder -> FilePath -> FilePath
dotDir DotRoot = id
dotDir DotFt = (</>) ".flare-timing"
dotDir DotFs = (</>) ".flight-system"
dotDir DotAs = (</>) ".air-score"

shape :: FileType -> FileShape

shape Fsdb = Ext ".fsdb"
shape CleanFsdb = DotDirName "clean-fsdb.xml" DotFs
shape TrimFsdb = DotDirName "trim-fsdb.xml" DotFs

shape Kml = Ext ".kml"
shape Igc = Ext ".igc"

shape CompInput = DotDirName "comp-input.yaml" DotFt
shape TaskLength = DotDirName "task-length.yaml" DotFt
shape CrossZone = DotDirName "cross-zone.yaml" DotFt
shape TagZone = DotDirName "tag-zone.yaml" DotFt
shape PegFrame = DotDirName "peg-frame.yaml" DotFt
shape LeadArea = DotDirName "lead-area.yaml" DotFt
shape MaskArrival = DotDirName "mask-arrival.yaml" DotFt
shape MaskEffort = DotDirName "mask-effort.yaml" DotFt
shape MaskLead = DotDirName "mask-lead.yaml" DotFt
shape MaskReach = DotDirName "mask-reach.yaml" DotFt
shape MaskSpeed = DotDirName "mask-speed.yaml" DotFt
shape BonusReach = DotDirName "bonus-reach.yaml" DotFt
shape LandOut = DotDirName "land-out.yaml" DotFt
shape FarOut = DotDirName "far-out.yaml" DotFt
shape GapPoint = DotDirName "gap-point.yaml" DotFt

shape UnpackTrack = Ext ".unpack-track.csv"
shape AlignTime = Ext ".align-time.csv"
shape DiscardFurther = Ext ".discard-further.csv"
shape PegThenDiscard = Ext ".peg-then-discard.csv"
shape AreaStep = Ext ".area-step.csv"

shape NormArrival = DotDirName "mask-arrival.yaml" DotFs
shape NormLandout = DotDirName "land-out.yaml" DotFs
shape NormRoute = DotDirName "task-route.yaml" DotFs
shape NormScore = DotDirName "gap-score.yaml" DotFs

dotDirTask :: CompDir -> DotFolder -> FilePath -> Int -> FilePath
dotDirTask (CompDir dir) dotFolder name task
    | DotFt <- dotFolder =
        dir </> ".flare-timing" </> name </> "task-" ++ show task
    | otherwise =
        error
        $ printf "Only %s has task folders but given %s" (show DotFt) (show dotFolder)

-- |
-- >>> fsdbToCleanFsdb (FsdbFile "a.fsdb")
-- CleanFsdbFile ".flight-system/clean-fsdb.xml"
--
-- prop> \s -> fsdbToCleanFsdb (FsdbFile s) == CleanFsdbFile ".flight-system/clean-fsdb.xml"
fsdbToCleanFsdb :: FsdbFile -> CleanFsdbFile
fsdbToCleanFsdb _ = let DotDirName s d = shape CleanFsdb in CleanFsdbFile $ dotDir d s

-- |
-- >>> cleanFsdbToTrimFsdb (CleanFsdbFile ".flight-system/clean-fsdb.xml")
-- TrimFsdbFile ".flight-system/trim-fsdb.xml"
--
-- prop> \s -> cleanFsdbToTrimFsdb (CleanFsdbFile s) == TrimFsdbFile ".flight-system/trim-fsdb.xml"
cleanFsdbToTrimFsdb :: CleanFsdbFile -> TrimFsdbFile
cleanFsdbToTrimFsdb _ = let DotDirName s d = shape TrimFsdb in TrimFsdbFile $ dotDir d s

-- |
-- >>> trimFsdbToComp (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- CompInputFile ".flare-timing/comp-input.yaml"
--
-- prop> \s -> trimFsdbToComp (TrimFsdbFile s) == CompInputFile ".flare-timing/comp-input.yaml"
trimFsdbToComp :: TrimFsdbFile -> CompInputFile
trimFsdbToComp _ = let DotDirName s d = shape CompInput in CompInputFile $ dotDir d s

-- |
-- >>> compToTaskLength (CompInputFile ".flare-timing/comp-input.yaml")
-- TaskLengthFile ".flare-timing/task-length.yaml"
--
-- prop> \s -> compToTaskLength (CompInputFile s) == TaskLengthFile ".flare-timing/task-length.yaml"
compToTaskLength :: CompInputFile -> TaskLengthFile
compToTaskLength _ = let DotDirName s d = shape TaskLength in TaskLengthFile $ dotDir d s

-- |
-- >>> compToCross (CompInputFile ".flare-timing/comp-input.yaml")
-- CrossZoneFile ".flare-timing/cross-zone.yaml"
--
-- prop> \s -> compToCross (CompInputFile s) == CrossZoneFile ".flare-timing/cross-zone.yaml"
compToCross :: CompInputFile -> CrossZoneFile
compToCross _ = let DotDirName s d = shape CrossZone in CrossZoneFile $ dotDir d s

-- |
-- >>> crossToTag (CrossZoneFile ".flare-timing/cross-zone.yaml")
-- TagZoneFile ".flare-timing/tag-zone.yaml"
--
-- prop> \s -> crossToTag (CrossZoneFile s) == TagZoneFile ".flare-timing/tag-zone.yaml"
crossToTag :: CrossZoneFile -> TagZoneFile
crossToTag _ = let DotDirName s d = shape TagZone in TagZoneFile $ dotDir d s

-- |
-- >>> tagToPeg (TagZoneFile ".flare-timing/tag-zone.yaml")
-- PegFrameFile ".flare-timing/peg-frame.yaml"
--
-- prop> \s -> tagToPeg (TagZoneFile s) == PegFrameFile ".flare-timing/peg-frame.yaml"
tagToPeg :: TagZoneFile -> PegFrameFile
tagToPeg _ = let DotDirName s d = shape PegFrame in PegFrameFile $ dotDir d s

-- |
-- >>> compToMaskArrival (CompInputFile ".flare-timing/comp-input.yaml")
-- MaskArrivalFile ".flare-timing/mask-arrival.yaml"
--
-- prop> \s -> compToMaskArrival (CompInputFile s) == MaskArrivalFile ".flare-timing/mask-arrival.yaml"
compToMaskArrival :: CompInputFile -> MaskArrivalFile
compToMaskArrival _ = let DotDirName s d = shape MaskArrival in MaskArrivalFile $ dotDir d s

-- |
-- >>> compToMaskEffort (CompInputFile ".flare-timing/comp-input.yaml")
-- MaskEffortFile ".flare-timing/mask-effort.yaml"
--
-- prop> \s -> compToMaskEffort (CompInputFile s) == MaskEffortFile ".flare-timing/mask-effort.yaml"
compToMaskEffort :: CompInputFile -> MaskEffortFile
compToMaskEffort _ = let DotDirName s d = shape MaskEffort in MaskEffortFile $ dotDir d s

-- |
-- >>> compToMaskLead (CompInputFile ".flare-timing/comp-input.yaml")
-- MaskLeadFile ".flare-timing/mask-lead.yaml"
--
-- prop> \s -> compToMaskLead (CompInputFile s) == MaskLeadFile ".flare-timing/mask-lead.yaml"
compToMaskLead :: CompInputFile -> MaskLeadFile
compToMaskLead _ = let DotDirName s d = shape MaskLead in MaskLeadFile $ dotDir d s

-- |
-- >>> compToMaskReach (CompInputFile ".flare-timing/comp-input.yaml")
-- MaskReachFile ".flare-timing/mask-reach.yaml"
--
-- prop> \s -> compToMaskReach (CompInputFile s) == MaskReachFile ".flare-timing/mask-reach.yaml"
compToMaskReach :: CompInputFile -> MaskReachFile
compToMaskReach _ = let DotDirName s d = shape MaskReach in MaskReachFile $ dotDir d s

-- |
-- >>> compToMaskSpeed (CompInputFile ".flare-timing/comp-input.yaml")
-- MaskSpeedFile ".flare-timing/mask-speed.yaml"
--
-- prop> \s -> compToMaskSpeed (CompInputFile s) == MaskSpeedFile ".flare-timing/mask-speed.yaml"
compToMaskSpeed :: CompInputFile -> MaskSpeedFile
compToMaskSpeed _ = let DotDirName s d = shape MaskSpeed in MaskSpeedFile $ dotDir d s

-- |
-- >>> compToBonusReach (CompInputFile ".flare-timing/comp-input.yaml")
-- BonusReachFile ".flare-timing/bonus-reach.yaml"
--
-- prop> \s -> compToBonusReach (CompInputFile s) == BonusReachFile ".flare-timing/bonus-reach.yaml"
compToBonusReach :: CompInputFile -> BonusReachFile
compToBonusReach _ = let DotDirName s d = shape BonusReach in BonusReachFile $ dotDir d s

-- |
-- >>> compToLeadArea (CompInputFile ".flare-timing/comp-input.yaml")
-- LeadAreaFile ".flare-timing/lead-area.yaml"
--
-- prop> \s -> compToLeadArea (CompInputFile s) == LeadAreaFile ".flare-timing/lead-area.yaml"
compToLeadArea :: CompInputFile -> LeadAreaFile
compToLeadArea _ = let DotDirName s d = shape LeadArea in LeadAreaFile $ dotDir d s

-- |
-- >>> compToLand (CompInputFile ".flare-timing/comp-input.yaml")
-- LandOutFile ".flare-timing/land-out.yaml"
--
-- prop> \s -> compToLand (CompInputFile s) == LandOutFile ".flare-timing/land-out.yaml"
compToLand :: CompInputFile -> LandOutFile
compToLand _ = let DotDirName s d = shape LandOut in LandOutFile $ dotDir d s

-- |
-- >>> compToFar (CompInputFile ".flare-timing/comp-input.yaml")
-- FarOutFile ".flare-timing/far-out.yaml"
--
-- prop> \s -> compToFar (CompInputFile s) == FarOutFile ".flare-timing/far-out.yaml"
compToFar :: CompInputFile -> FarOutFile
compToFar _ = let DotDirName s d = shape FarOut in FarOutFile $ dotDir d s

-- |
-- >>> compToPoint (CompInputFile ".flare-timing/comp-input.yaml")
-- GapPointFile ".flare-timing/gap-point.yaml"
--
-- prop> \s -> compToPoint (CompInputFile s) == GapPointFile ".flare-timing/gap-point.yaml"
compToPoint :: CompInputFile -> GapPointFile
compToPoint _ = let DotDirName s d = shape GapPoint in GapPointFile $ dotDir d s

-- |
-- >>> compToNormArrival (CompInputFile ".flare-timing/comp-input.yaml")
-- NormArrivalFile ".flight-system/mask-arrival.yaml"
--
-- prop> \s -> compToNormArrival (CompInputFile s) == NormArrivalFile ".flight-system/mask-arrival.yaml"
compToNormArrival :: CompInputFile -> NormArrivalFile
compToNormArrival _ = let DotDirName s d = shape NormArrival in NormArrivalFile $ dotDir d s

-- |
-- >>> compToNormLandout (CompInputFile ".flare-timing/comp-input.yaml")
-- NormLandoutFile ".flight-system/land-out.yaml"
--
-- prop> \s -> compToNormLandout (CompInputFile s) == NormLandoutFile ".flight-system/land-out.yaml"
compToNormLandout :: CompInputFile -> NormLandoutFile
compToNormLandout _ = let DotDirName s d = shape NormLandout in NormLandoutFile $ dotDir d s

-- |
-- >>> trimFsdbToNormArrival (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- NormArrivalFile ".flight-system/mask-arrival.yaml"
--
-- prop> \s -> trimFsdbToNormArrival (TrimFsdbFile s) == NormArrivalFile ".flight-system/mask-arrival.yaml"
trimFsdbToNormArrival :: TrimFsdbFile -> NormArrivalFile
trimFsdbToNormArrival _ = let DotDirName s d = shape NormArrival in NormArrivalFile $ dotDir d s

-- |
-- >>> trimFsdbToNormLandout (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- NormLandoutFile ".flight-system/land-out.yaml"
--
-- prop> \s -> trimFsdbToNormLandout (TrimFsdbFile s) == NormLandoutFile ".flight-system/land-out.yaml"
trimFsdbToNormLandout :: TrimFsdbFile -> NormLandoutFile
trimFsdbToNormLandout _ = let DotDirName s d = shape NormLandout in NormLandoutFile $ dotDir d s

-- |
-- >>> compToNormRoute (CompInputFile ".flare-timing/comp-input.yaml")
-- NormRouteFile ".flight-system/task-route.yaml"
--
-- prop> \s -> compToNormRoute (CompInputFile s) == NormRouteFile ".flight-system/task-route.yaml"
compToNormRoute :: CompInputFile -> NormRouteFile
compToNormRoute _ = let DotDirName s d = shape NormRoute in NormRouteFile $ dotDir d s

-- |
-- >>> trimFsdbToNormRoute (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- NormRouteFile ".flight-system/task-route.yaml"
--
-- prop> \s -> trimFsdbToNormRoute (TrimFsdbFile s) == NormRouteFile ".flight-system/task-route.yaml"
trimFsdbToNormRoute :: TrimFsdbFile -> NormRouteFile
trimFsdbToNormRoute _ = let DotDirName s d = shape NormRoute in NormRouteFile $ dotDir d s

-- |
-- >>> compToNormScore (CompInputFile ".flare-timing/comp-input.yaml")
-- NormScoreFile ".flight-system/gap-score.yaml"
--
-- prop> \s -> compToNormScore (CompInputFile s) == NormScoreFile ".flight-system/gap-score.yaml"
compToNormScore :: CompInputFile -> NormScoreFile
compToNormScore _ = let DotDirName s d = shape NormScore in NormScoreFile $ dotDir d s

-- |
-- >>> trimFsdbToNormScore (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- NormScoreFile ".flight-system/gap-score.yaml"
--
-- prop> \s -> trimFsdbToNormScore (TrimFsdbFile s) == NormScoreFile ".flight-system/gap-score.yaml"
trimFsdbToNormScore :: TrimFsdbFile -> NormScoreFile
trimFsdbToNormScore _ = let DotDirName s d = shape NormScore in NormScoreFile $ dotDir d s

compFileToCompDir :: CompInputFile -> CompDir
compFileToCompDir (CompInputFile p) =
    CompDir $ takeDirectory p

pilotPath :: Pilot -> FilePath
pilotPath (Pilot (PilotId k, PilotName s)) =
    s ++ " " ++ k

-- |
-- >>> unpackTrackPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- (UnpackTrackDir "a/.flare-timing/unpack-track/task-1",UnpackTrackFile "Frodo 101.csv")
unpackTrackPath :: CompDir -> Int -> Pilot -> (UnpackTrackDir, UnpackTrackFile)
unpackTrackPath dir task pilot =
    (unpackTrackDir dir task, UnpackTrackFile $ pilotPath pilot <.> "csv")

unpackTrackDir :: CompDir -> Int -> UnpackTrackDir
unpackTrackDir comp task =
    UnpackTrackDir $ dotDirTask comp DotFt "unpack-track" task

-- |
-- >>> alignTimePath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- (AlignTimeDir "a/.flare-timing/align-time/task-1",AlignTimeFile "Frodo 101.csv")
alignTimePath :: CompDir -> Int -> Pilot -> (AlignTimeDir, AlignTimeFile)
alignTimePath dir task pilot =
    (alignTimeDir dir task, AlignTimeFile $ pilotPath pilot <.> "csv")

alignTimeDir :: CompDir -> Int -> AlignTimeDir
alignTimeDir comp task =
    AlignTimeDir $ dotDirTask comp DotFt "align-time" task

-- |
-- >>> discardFurtherPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- (DiscardFurtherDir "a/.flare-timing/discard-further/task-1",DiscardFurtherFile "Frodo 101.csv")
discardFurtherPath :: CompDir -> Int -> Pilot -> (DiscardFurtherDir, DiscardFurtherFile)
discardFurtherPath dir task pilot =
    (discardFurtherDir dir task, DiscardFurtherFile $ pilotPath pilot <.> "csv")

discardFurtherDir :: CompDir -> Int -> DiscardFurtherDir
discardFurtherDir comp task =
    DiscardFurtherDir $ dotDirTask comp DotFt "discard-further" task

-- |
-- >>> pegThenDiscardPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- (PegThenDiscardDir "a/.flare-timing/peg-then-discard/task-1",PegThenDiscardFile "Frodo 101.csv")
pegThenDiscardPath :: CompDir -> Int -> Pilot -> (PegThenDiscardDir, PegThenDiscardFile)
pegThenDiscardPath dir task pilot =
    (pegThenDiscardDir dir task, PegThenDiscardFile $ pilotPath pilot <.> "csv")

pegThenDiscardDir :: CompDir -> Int -> PegThenDiscardDir
pegThenDiscardDir comp task =
    PegThenDiscardDir $ dotDirTask comp DotFt "peg-then-discard" task

-- |
-- >>> areaStepPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- (AreaStepDir "a/.flare-timing/area-step/task-1",AreaStepFile "Frodo 101.csv")
areaStepPath :: CompDir -> Int -> Pilot -> (AreaStepDir, AreaStepFile)
areaStepPath dir task pilot =
    (areaStepDir dir task, AreaStepFile $ pilotPath pilot <.> "csv")

areaStepDir :: CompDir -> Int -> AreaStepDir
areaStepDir comp task =
    AreaStepDir $ dotDirTask comp DotFt "area-step" task

-- $setup
-- >>> import Test.QuickCheck
