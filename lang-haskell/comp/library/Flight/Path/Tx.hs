module Flight.Path.Tx
    ( trimFsdbToAltArrival
    , trimFsdbToAltLandout
    , trimFsdbToAltRoute
    , trimFsdbToAltScore
    , compToAltArrival
    , compToAltLandout
    , compToAltRoute
    , compToAltScore
    , fsdbToCleanFsdb
    , cleanFsdbToTrimFsdb
    , trimFsdbToComp

    , taskToTaskLength
    , taskToFlyTime
    , taskToCrossZone
    , taskToTagZone
    , taskToPegFrame
    , taskToLeadArea
    , taskToMaskArrival
    , taskToMaskEffort
    , taskToMaskReach
    , taskToMaskBonus
    , taskToMaskSpeed
    , taskToMaskLead
    , taskToLandOut
    , taskToFarOut
    , taskToGapPoint

    , compFileToCompDir
    , taskDir
    , unpackTrackDir
    , alignTimeDir
    , discardFurtherDir
    , pegThenDiscardDir
    , areaStepDir

    , taskInputPath
    , taskLengthPath
    , flyTimePath
    , crossZonePath
    , tagZonePath
    , pegFramePath
    , leadAreaPath
    , maskArrivalPath
    , maskEffortPath
    , maskReachPath
    , maskBonusPath
    , maskSpeedPath
    , maskLeadPath
    , landOutPath
    , farOutPath
    , gapPointPath

    , unpackTrackPath
    , alignTimePath
    , discardFurtherPath
    , pegThenDiscardPath
    , areaStepPath
    , reshape
    ) where

import Data.Coerce (coerce)
import Text.Printf (printf)
import System.FilePath (FilePath, (</>), (<.>), takeDirectory, replaceExtensions)
import "flight-gap-allot" Flight.Score (PilotId(..), PilotName(..), Pilot(..))
import Flight.Path.Types

dotDir :: DotFolder -> FilePath -> FilePath
dotDir DotRoot = id
dotDir DotFt = (</>) ".flare-timing"
dotDir DotFs = (</>) ".flight-system"
dotDir DotAs = (</>) ".air-score"

shape :: FileType -> FileShape

shape Fsdb = Ext ".fsdb"
shape CleanFsdb = Ext ".clean-fsdb.xml"
shape TrimFsdb = DotDirName "trim-fsdb.xml" DotFt

shape Kml = Ext ".kml"
shape Igc = Ext ".igc"

shape CompInput = DotDirName "comp-input.yaml" DotFt
shape TaskInput = DotDirName "task-input.yaml" DotFt
shape TaskLength = DotDirName "task-length.yaml" DotFt
shape FlyTime = DotDirName "fly-time.yaml" DotFt
shape CrossZone = DotDirName "cross-zone.yaml" DotFt
shape TagZone = DotDirName "tag-zone.yaml" DotFt
shape PegFrame = DotDirName "peg-frame.yaml" DotFt
shape LeadArea = DotDirName "lead-area.yaml" DotFt
shape MaskArrival = DotDirName "mask-arrival.yaml" DotFt
shape MaskEffort = DotDirName "mask-effort.yaml" DotFt
shape MaskLead = DotDirName "mask-lead.yaml" DotFt
shape MaskReach = DotDirName "mask-reach.yaml" DotFt
shape MaskSpeed = DotDirName "mask-speed.yaml" DotFt
shape MaskBonus = DotDirName "bonus-reach.yaml" DotFt
shape LandOut = DotDirName "land-out.yaml" DotFt
shape FarOut = DotDirName "far-out.yaml" DotFt
shape GapPoint = DotDirName "gap-point.yaml" DotFt

shape UnpackTrack = Ext ".unpack-track.csv"
shape AlignTime = Ext ".align-time.csv"
shape DiscardFurther = Ext ".discard-further.csv"
shape PegThenDiscard = Ext ".peg-then-discard.csv"
shape AreaStep = Ext ".area-step.csv"

shape (AltArrival AltFs) = DotDirName "mask-arrival.yaml" DotFs
shape (AltLandout AltFs) = DotDirName "land-out.yaml" DotFs
shape (AltRoute AltFs) = DotDirName "task-route.yaml" DotFs
shape (AltScore AltFs) = DotDirName "gap-score.yaml" DotFs

shape (AltArrival AltAs) = DotDirName "mask-arrival.yaml" DotAs
shape (AltLandout AltAs) = DotDirName "land-out.yaml" DotAs
shape (AltRoute AltAs) = DotDirName "task-route.yaml" DotAs
shape (AltScore AltAs) = DotDirName "gap-score.yaml" DotAs

-- |
-- >>> reshape TaskInput ".flare-timing/comp-input.yaml"
-- "task-input.yaml"
reshape :: FileType -> FilePath -> FilePath
reshape Fsdb = flip replaceExtensions "fsdb"
reshape CleanFsdb = flip replaceExtensions "clean-fsdb.xml"
reshape TrimFsdb = coerce . cleanFsdbToTrimFsdb . fsdbToCleanFsdb . FsdbFile

reshape Kml = id
reshape Igc = id

reshape CompInput = coerce . trimFsdbToComp . coerce . reshape TrimFsdb
reshape TaskInput = const "task-input.yaml"
reshape TaskLength = const "task-length.yaml"
reshape FlyTime = const "fly-time.yaml"
reshape CrossZone = const "cross-zone.yaml"
reshape TagZone = const "tag-zone.yaml"
reshape PegFrame = const "peg-frame.yaml"
reshape LeadArea = const "lead-area.yaml"

reshape MaskArrival = const "mask-arrival.yaml"
reshape MaskEffort = const "mask-effort.yaml"
reshape MaskLead = const "mask-lead.yaml"
reshape MaskReach = const "mask-reach.yaml"
reshape MaskSpeed = const "mask-speed.yaml"

reshape MaskBonus = const "mask-bonus.yaml"
reshape LandOut = const "land-out.yaml"
reshape FarOut = const "far-out.yaml"
reshape GapPoint = const "gap-point.yaml"

reshape UnpackTrack = flip replaceExtensions "unpack-track.csv"
reshape AlignTime = flip replaceExtensions "align-time.csv"
reshape DiscardFurther = flip replaceExtensions "discard-further.csv"
reshape PegThenDiscard = flip replaceExtensions "peg-then-discard.csv"
reshape AreaStep = flip replaceExtensions "area-step.csv"

reshape (AltArrival x) = coerce . compToAltArrival x . coerce . reshape CompInput
reshape (AltLandout x) = coerce . compToAltLandout x . coerce . reshape CompInput
reshape (AltRoute x) = coerce . compToAltRoute x . coerce . reshape CompInput
reshape (AltScore x) = coerce . compToAltScore x . coerce . reshape CompInput

dotDirTask :: CompDir -> DotFolder -> IxTask -> FilePath
dotDirTask (CompDir dir) dotFolder (IxTask task)
    | DotFt <- dotFolder = dir </> ".flare-timing" </> "task-" ++ show task
    | otherwise =
        error
        $ printf "Only %s has task folders but given %s" (show DotFt) (show dotFolder)

dotSubdirTask :: CompDir -> DotFolder -> FilePath -> IxTask -> FilePath
dotSubdirTask comp dotFolder name task
    | DotFt <- dotFolder = dotDirTask comp dotFolder task </> name
    | otherwise =
        error
        $ printf "Only %s has task folders but given %s" (show DotFt) (show dotFolder)

-- |
-- >>> fsdbToCleanFsdb (FsdbFile "a.fsdb")
-- "a.clean-fsdb.xml"
--
-- prop> \s -> not (elem '.' s) ==> fsdbToCleanFsdb (FsdbFile $ s ++ ".fsdb") == (CleanFsdbFile $ s ++ ".clean-fsdb.xml")
fsdbToCleanFsdb :: FsdbFile -> CleanFsdbFile
fsdbToCleanFsdb (FsdbFile x) = CleanFsdbFile $ reshape CleanFsdb x

-- |
-- >>> cleanFsdbToTrimFsdb (CleanFsdbFile "a.clean-fsdb.xml")
-- ".flare-timing/trim-fsdb.xml"
--
-- prop> \s -> cleanFsdbToTrimFsdb (CleanFsdbFile s) == TrimFsdbFile ".flare-timing/trim-fsdb.xml"
cleanFsdbToTrimFsdb :: CleanFsdbFile -> TrimFsdbFile
cleanFsdbToTrimFsdb _ = let DotDirName s d = shape TrimFsdb in TrimFsdbFile $ dotDir d s

-- |
-- >>> trimFsdbToComp (TrimFsdbFile ".flare-timing/trim-fsdb.xml")
-- ".flare-timing/comp-input.yaml"
--
-- prop> \s -> trimFsdbToComp (TrimFsdbFile s) == CompInputFile ".flare-timing/comp-input.yaml"
trimFsdbToComp :: TrimFsdbFile -> CompInputFile
trimFsdbToComp _ = let DotDirName s d = shape CompInput in CompInputFile $ dotDir d s

-- |
-- >>> taskToTaskLength (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/task-length.yaml"
taskToTaskLength :: TaskInputFile -> TaskLengthFile
taskToTaskLength (TaskInputFile s) = TaskLengthFile $ takeDirectory s </> reshape TaskLength s

-- |
-- >>> taskToFlyTime (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/fly-time.yaml"
taskToFlyTime :: TaskInputFile -> FlyTimeFile
taskToFlyTime (TaskInputFile s) = FlyTimeFile $ takeDirectory s </> reshape FlyTime s

-- |
-- >>> taskToCrossZone (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/cross-zone.yaml"
taskToCrossZone :: TaskInputFile -> CrossZoneFile
taskToCrossZone (TaskInputFile s) = CrossZoneFile $ takeDirectory s </> reshape CrossZone s

-- |
-- >>> taskToTagZone (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/tag-zone.yaml"
taskToTagZone :: TaskInputFile -> TagZoneFile
taskToTagZone (TaskInputFile s) = TagZoneFile $ takeDirectory s </> reshape TagZone s

-- |
-- >>> taskToPegFrame (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/peg-frame.yaml"
taskToPegFrame :: TaskInputFile -> PegFrameFile
taskToPegFrame (TaskInputFile s) = PegFrameFile $ takeDirectory s </> reshape PegFrame s

-- |
-- >>> taskToLeadArea (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/lead-area.yaml"
taskToLeadArea :: TaskInputFile -> LeadAreaFile
taskToLeadArea (TaskInputFile s) = LeadAreaFile $ takeDirectory s </> reshape LeadArea s

-- |
-- >>> taskToMaskArrival (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/mask-arrival.yaml"
taskToMaskArrival :: TaskInputFile -> MaskArrivalFile
taskToMaskArrival (TaskInputFile s) = MaskArrivalFile $ takeDirectory s </> reshape MaskArrival s

-- |
-- >>> taskToMaskEffort (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/mask-effort.yaml"
taskToMaskEffort :: TaskInputFile -> MaskEffortFile
taskToMaskEffort (TaskInputFile s) = MaskEffortFile $ takeDirectory s </> reshape MaskEffort s

-- |
-- >>> taskToMaskReach (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/mask-reach.yaml"
taskToMaskReach :: TaskInputFile -> MaskReachFile
taskToMaskReach (TaskInputFile s) = MaskReachFile $ takeDirectory s </> reshape MaskReach s

-- |
-- >>> taskToMaskBonus (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/mask-bonus.yaml"
taskToMaskBonus :: TaskInputFile -> MaskBonusFile
taskToMaskBonus (TaskInputFile s) = MaskBonusFile $ takeDirectory s </> reshape MaskBonus s

-- |
-- >>> taskToMaskSpeed (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/mask-speed.yaml"
taskToMaskSpeed :: TaskInputFile -> MaskSpeedFile
taskToMaskSpeed (TaskInputFile s) = MaskSpeedFile $ takeDirectory s </> reshape MaskSpeed s

-- |
-- >>> taskToMaskLead (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/mask-lead.yaml"
taskToMaskLead :: TaskInputFile -> MaskLeadFile
taskToMaskLead (TaskInputFile s) = MaskLeadFile $ takeDirectory s </> reshape MaskLead s

-- |
-- >>> taskToLandOut (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/land-out.yaml"
taskToLandOut :: TaskInputFile -> LandOutFile
taskToLandOut (TaskInputFile s) = LandOutFile $ takeDirectory s </> reshape LandOut s

-- |
-- >>> taskToFarOut (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/far-out.yaml"
taskToFarOut :: TaskInputFile -> FarOutFile
taskToFarOut (TaskInputFile s) = FarOutFile $ takeDirectory s </> reshape FarOut s

-- |
-- >>> taskToGapPoint (TaskInputFile ".flare-timing/task-1/task-input.yaml")
-- ".flare-timing/task-1/gap-point.yaml"
taskToGapPoint :: TaskInputFile -> GapPointFile
taskToGapPoint (TaskInputFile s) = GapPointFile $ takeDirectory s </> reshape GapPoint s

-- |
-- >>> compToAltArrival AltFs (CompInputFile ".flare-timing/comp-input.yaml")
-- ".flight-system/mask-arrival.yaml"
--
-- prop> \s -> compToAltArrival AltFs (CompInputFile s) == AltArrivalFile ".flight-system/mask-arrival.yaml"
compToAltArrival :: AltDot -> CompInputFile -> AltArrivalFile
compToAltArrival a _ = let DotDirName s d = shape (AltArrival a) in
    AltArrivalFile $ dotDir d s

-- |
-- >>> compToAltLandout AltFs (CompInputFile ".flare-timing/comp-input.yaml")
-- ".flight-system/land-out.yaml"
--
-- prop> \s -> compToAltLandout AltFs (CompInputFile s) == AltLandoutFile ".flight-system/land-out.yaml"
compToAltLandout :: AltDot -> CompInputFile -> AltLandoutFile
compToAltLandout a _ = let DotDirName s d = shape (AltLandout a) in
    AltLandoutFile $ dotDir d s

-- |
-- >>> trimFsdbToAltArrival AltFs (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- ".flight-system/mask-arrival.yaml"
--
-- prop> \s -> trimFsdbToAltArrival AltFs (TrimFsdbFile s) == AltArrivalFile ".flight-system/mask-arrival.yaml"
trimFsdbToAltArrival :: AltDot -> TrimFsdbFile -> AltArrivalFile
trimFsdbToAltArrival a _ = let DotDirName s d = shape (AltArrival a) in
    AltArrivalFile $ dotDir d s

-- |
-- >>> trimFsdbToAltLandout AltFs (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- ".flight-system/land-out.yaml"
--
-- prop> \s -> trimFsdbToAltLandout AltFs (TrimFsdbFile s) == AltLandoutFile ".flight-system/land-out.yaml"
trimFsdbToAltLandout :: AltDot -> TrimFsdbFile -> AltLandoutFile
trimFsdbToAltLandout a _ = let DotDirName s d = shape (AltLandout a) in
    AltLandoutFile $ dotDir d s

-- |
-- >>> compToAltRoute AltFs (CompInputFile ".flare-timing/comp-input.yaml")
-- ".flight-system/task-route.yaml"
--
-- prop> \s -> compToAltRoute AltFs (CompInputFile s) == AltRouteFile ".flight-system/task-route.yaml"
compToAltRoute :: AltDot -> CompInputFile -> AltRouteFile
compToAltRoute a _ = let DotDirName s d = shape (AltRoute a) in
    AltRouteFile $ dotDir d s

-- |
-- >>> trimFsdbToAltRoute AltFs (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- ".flight-system/task-route.yaml"
--
-- prop> \s -> trimFsdbToAltRoute AltFs (TrimFsdbFile s) == AltRouteFile ".flight-system/task-route.yaml"
trimFsdbToAltRoute :: AltDot -> TrimFsdbFile -> AltRouteFile
trimFsdbToAltRoute a _ = let DotDirName s d = shape (AltRoute a) in
    AltRouteFile $ dotDir d s

-- |
-- >>> compToAltScore AltFs (CompInputFile ".flare-timing/comp-input.yaml")
-- ".flight-system/gap-score.yaml"
--
-- >>> compToAltScore AltAs (CompInputFile ".flare-timing/comp-input.yaml")
-- ".air-score/gap-score.yaml"
--
-- prop> \s -> compToAltScore AltFs (CompInputFile s) == AltScoreFile ".flight-system/gap-score.yaml"
-- prop> \s -> compToAltScore AltAs (CompInputFile s) == AltScoreFile ".air-score/gap-score.yaml"
compToAltScore :: AltDot -> CompInputFile -> AltScoreFile
compToAltScore a _ = let DotDirName s d = shape (AltScore a) in
    AltScoreFile $ dotDir d s

-- |
-- >>> trimFsdbToAltScore AltFs (TrimFsdbFile ".flight-system/trim-fsdb.xml")
-- ".flight-system/gap-score.yaml"
--
-- prop> \s -> trimFsdbToAltScore AltFs (TrimFsdbFile s) == AltScoreFile ".flight-system/gap-score.yaml"
trimFsdbToAltScore :: AltDot -> TrimFsdbFile -> AltScoreFile
trimFsdbToAltScore a _ = let DotDirName s d = shape (AltScore a) in
    AltScoreFile $ dotDir d s

compFileToCompDir :: CompInputFile -> CompDir
compFileToCompDir (CompInputFile p) = CompDir . takeDirectory $ takeDirectory p

pilotPath :: Pilot -> FilePath
pilotPath (Pilot (PilotId k, PilotName s)) =
    s ++ " " ++ k

taskDir :: CompDir -> IxTask -> TaskDir
taskDir comp task = TaskDir $ dotDirTask comp DotFt task

-- |
-- >>> taskInputPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","task-input.yaml")
taskInputPath :: CompDir -> IxTask -> (TaskDir, TaskInputFile)
taskInputPath dir task = (taskDir dir task, TaskInputFile "task-input.yaml")

-- |
-- >>> taskLengthPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","task-length.yaml")
taskLengthPath :: CompDir -> IxTask -> (TaskDir, TaskLengthFile)
taskLengthPath dir task = (taskDir dir task, TaskLengthFile "task-length.yaml")

-- |
-- >>> flyTimePath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","fly-time.yaml")
flyTimePath :: CompDir -> IxTask -> (TaskDir, FlyTimeFile)
flyTimePath dir task = (taskDir dir task, FlyTimeFile "fly-time.yaml")

-- |
-- >>> crossZonePath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","cross-zone.yaml")
crossZonePath :: CompDir -> IxTask -> (TaskDir, CrossZoneFile)
crossZonePath dir task = (taskDir dir task, CrossZoneFile "cross-zone.yaml")

-- |
-- >>> tagZonePath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","tag-zone.yaml")
tagZonePath :: CompDir -> IxTask -> (TaskDir, TagZoneFile)
tagZonePath dir task = (taskDir dir task, TagZoneFile "tag-zone.yaml")

-- |
-- >>> pegFramePath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","peg-frame.yaml")
pegFramePath :: CompDir -> IxTask -> (TaskDir, PegFrameFile)
pegFramePath dir task = (taskDir dir task, PegFrameFile "peg-frame.yaml")

-- |
-- >>> leadAreaPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","lead-area.yaml")
leadAreaPath :: CompDir -> IxTask -> (TaskDir, LeadAreaFile)
leadAreaPath dir task = (taskDir dir task, LeadAreaFile "lead-area.yaml")

-- |
-- >>> maskArrivalPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","mask-arrival.yaml")
maskArrivalPath :: CompDir -> IxTask -> (TaskDir, MaskArrivalFile)
maskArrivalPath dir task = (taskDir dir task, MaskArrivalFile "mask-arrival.yaml")

-- |
-- >>> maskEffortPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","mask-effort.yaml")
maskEffortPath :: CompDir -> IxTask -> (TaskDir, MaskEffortFile)
maskEffortPath dir task = (taskDir dir task, MaskEffortFile "mask-effort.yaml")

-- |
-- >>> maskReachPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","mask-reach.yaml")
maskReachPath :: CompDir -> IxTask -> (TaskDir, MaskReachFile)
maskReachPath dir task = (taskDir dir task, MaskReachFile "mask-reach.yaml")

-- |
-- >>> maskBonusPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","mask-bonus.yaml")
maskBonusPath :: CompDir -> IxTask -> (TaskDir, MaskBonusFile)
maskBonusPath dir task = (taskDir dir task, MaskBonusFile "mask-bonus.yaml")

-- |
-- >>> maskSpeedPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","mask-speed.yaml")
maskSpeedPath :: CompDir -> IxTask -> (TaskDir, MaskSpeedFile)
maskSpeedPath dir task = (taskDir dir task, MaskSpeedFile "mask-speed.yaml")

-- |
-- >>> maskLeadPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","mask-lead.yaml")
maskLeadPath :: CompDir -> IxTask -> (TaskDir, MaskLeadFile)
maskLeadPath dir task = (taskDir dir task, MaskLeadFile "mask-lead.yaml")

-- |
-- >>> landOutPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","land-out.yaml")
landOutPath :: CompDir -> IxTask -> (TaskDir, LandOutFile)
landOutPath dir task = (taskDir dir task, LandOutFile "land-out.yaml")

-- |
-- >>> farOutPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","far-out.yaml")
farOutPath :: CompDir -> IxTask -> (TaskDir, FarOutFile)
farOutPath dir task = (taskDir dir task, FarOutFile "far-out.yaml")

-- |
-- >>> gapPointPath (CompDir "a") (IxTask 1)
-- ("a/.flare-timing/task-1","gap-point.yaml")
gapPointPath :: CompDir -> IxTask -> (TaskDir, FarOutFile)
gapPointPath dir task = (taskDir dir task, FarOutFile "gap-point.yaml")

-- |
-- >>> unpackTrackPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- ("a/.flare-timing/task-1/unpack-track","Frodo 101.csv")
unpackTrackPath :: CompDir -> IxTask -> Pilot -> (UnpackTrackDir, UnpackTrackFile)
unpackTrackPath dir task pilot =
    (unpackTrackDir dir task, UnpackTrackFile $ pilotPath pilot <.> "csv")

unpackTrackDir :: CompDir -> IxTask -> UnpackTrackDir
unpackTrackDir comp task =
    UnpackTrackDir $ dotSubdirTask comp DotFt "unpack-track" task

-- |
-- >>> alignTimePath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- ("a/.flare-timing/task-1/align-time","Frodo 101.csv")
alignTimePath :: CompDir -> IxTask -> Pilot -> (AlignTimeDir, AlignTimeFile)
alignTimePath dir task pilot =
    (alignTimeDir dir task, AlignTimeFile $ pilotPath pilot <.> "csv")

alignTimeDir :: CompDir -> IxTask -> AlignTimeDir
alignTimeDir comp task =
    AlignTimeDir $ dotSubdirTask comp DotFt "align-time" task

-- |
-- >>> discardFurtherPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- ("a/.flare-timing/task-1/discard-further","Frodo 101.csv")
discardFurtherPath :: CompDir -> IxTask -> Pilot -> (DiscardFurtherDir, DiscardFurtherFile)
discardFurtherPath dir task pilot =
    (discardFurtherDir dir task, DiscardFurtherFile $ pilotPath pilot <.> "csv")

discardFurtherDir :: CompDir -> IxTask -> DiscardFurtherDir
discardFurtherDir comp task =
    DiscardFurtherDir $ dotSubdirTask comp DotFt "discard-further" task

-- |
-- >>> pegThenDiscardPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- ("a/.flare-timing/task-1/peg-then-discard","Frodo 101.csv")
pegThenDiscardPath :: CompDir -> IxTask -> Pilot -> (PegThenDiscardDir, PegThenDiscardFile)
pegThenDiscardPath dir task pilot =
    (pegThenDiscardDir dir task, PegThenDiscardFile $ pilotPath pilot <.> "csv")

pegThenDiscardDir :: CompDir -> IxTask -> PegThenDiscardDir
pegThenDiscardDir comp task =
    PegThenDiscardDir $ dotSubdirTask comp DotFt "peg-then-discard" task

-- |
-- >>> areaStepPath (CompDir "a") 1 (Pilot (PilotId "101", PilotName "Frodo"))
-- ("a/.flare-timing/task-1/area-step","Frodo 101.csv")
areaStepPath :: CompDir -> IxTask -> Pilot -> (AreaStepDir, AreaStepFile)
areaStepPath dir task pilot =
    (areaStepDir dir task, AreaStepFile $ pilotPath pilot <.> "csv")

areaStepDir :: CompDir -> IxTask -> AreaStepDir
areaStepDir comp task =
    AreaStepDir $ dotSubdirTask comp DotFt "area-step" task

-- $setup
-- >>> import Test.QuickCheck
