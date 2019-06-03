module Flight.Path
    ( FileType(..)
    , IgcFile(..)
    , KmlFile(..)
    , FsdbFile(..)
    , FsdbXml(..)
    , CompInputFile(..)
    , NormScoreFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , PegFrameFile(..)
    , UnpackTrackFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , PegThenDiscardFile(..)
    , MaskArrivalFile(..)
    , MaskEffortFile(..)
    , MaskLeadFile(..)
    , MaskReachFile(..)
    , MaskSpeedFile(..)
    , BonusReachFile(..)
    , LandOutFile(..)
    , GapPointFile(..)
    , CompDir(..)
    , UnpackTrackDir(..)
    , AlignTimeDir(..)
    , DiscardFurtherDir(..)
    , PegThenDiscardDir(..)
    , fsdbToComp
    , fsdbToScore
    , compToScore
    , compToTaskLength
    , compToCross
    , compToMaskArrival
    , compToMaskEffort
    , compToMaskLead
    , compToMaskReach
    , compToMaskSpeed
    , compToBonusReach
    , compToLand
    , compToPoint
    , crossToTag
    , tagToPeg
    , compFileToCompDir
    , unpackTrackDir
    , alignTimeDir
    , discardFurtherDir
    , pegThenDiscardDir
    , unpackTrackPath
    , alignTimePath
    , discardFurtherPath
    , findFsdb
    , findCompInput
    , findNormScore
    , findCrossZone
    , findIgc
    , findKml
    , ext
    , ensureExt
    ) where

import GHC.Records
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath
    ( FilePath, (</>), (<.>)
    , replaceExtensions, replaceExtension, dropExtension, takeDirectory
    )
import System.FilePath.Find
    ((==?), (&&?), find, always, fileType, extension)
import qualified System.FilePath.Find as Find (FileType(..))
import Flight.Score (PilotId(..), PilotName(..), Pilot(..))

-- | The path to a *.igc file.
newtype IgcFile = IgcFile FilePath

-- | The path to a *.kml file.
newtype KmlFile = KmlFile FilePath

-- | The path to a competition *.fsdb file.
newtype FsdbFile = FsdbFile FilePath

-- | The XML string contents of a *.fsdb file.
newtype FsdbXml = FsdbXml String

-- | The directory path of a competition file.
newtype CompDir = CompDir FilePath

-- | The path to a competition file.
newtype CompInputFile = CompInputFile FilePath

-- | The path to a competition expected score file.
newtype NormScoreFile = NormScoreFile FilePath

-- | The path to a task length file.
newtype TaskLengthFile = TaskLengthFile FilePath

-- | The path to a cross zone file.
newtype CrossZoneFile = CrossZoneFile FilePath

-- | The path to a tag zone file.
newtype TagZoneFile = TagZoneFile FilePath

-- | The path to a stop task file.
newtype PegFrameFile = PegFrameFile FilePath

-- | The path to an unpack track directory for a single task.
newtype UnpackTrackDir = UnpackTrackDir FilePath

-- | The path to an align time directory for a single task.
newtype AlignTimeDir = AlignTimeDir FilePath

-- | The path to a discard further directory for a single task.
newtype DiscardFurtherDir = DiscardFurtherDir FilePath

-- | The path to a peg then discard directory for a single task.
newtype PegThenDiscardDir = PegThenDiscardDir FilePath

-- | The path to as unpack track file.
newtype UnpackTrackFile = UnpackTrackFile FilePath

-- | The path to an align time file.
newtype AlignTimeFile = AlignTimeFile FilePath

-- | The path to a discard further file.
newtype DiscardFurtherFile = DiscardFurtherFile FilePath

-- | The path to a peg then discard file.
newtype PegThenDiscardFile = PegThenDiscardFile FilePath

-- | The path to a mask arrival file.
newtype MaskArrivalFile = MaskArrivalFile FilePath

-- | The path to a mask effort file.
newtype MaskEffortFile = MaskEffortFile FilePath

-- | The path to a mask lead file.
newtype MaskLeadFile = MaskLeadFile FilePath

-- | The path to a mask reach file.
newtype MaskReachFile = MaskReachFile FilePath

-- | The path to as mask speed file.
newtype MaskSpeedFile = MaskSpeedFile FilePath

-- | The path to a mask reach with altitude bonus distance file.
newtype BonusReachFile = BonusReachFile FilePath

-- | The path to as land out file.
newtype LandOutFile = LandOutFile FilePath

-- | The path to as gap point file.
newtype GapPointFile = GapPointFile FilePath

fsdbToComp :: FsdbFile -> CompInputFile
fsdbToComp (FsdbFile p) =
    CompInputFile $ replaceExtension p (ext CompInput)

fsdbToScore :: FsdbFile -> NormScoreFile
fsdbToScore (FsdbFile p) =
    NormScoreFile $ replaceExtension p (ext NormScore)

compToScore :: CompInputFile -> NormScoreFile
compToScore (CompInputFile p) =
    NormScoreFile $ flip replaceExtension (ext NormScore) $ dropExtension p

compFileToCompDir :: CompInputFile -> CompDir
compFileToCompDir (CompInputFile p) =
    CompDir $ takeDirectory p

compToTaskLength :: CompInputFile -> TaskLengthFile
compToTaskLength (CompInputFile p) =
    TaskLengthFile $ flip replaceExtension (ext TaskLength) $ dropExtension p

compToCross :: CompInputFile -> CrossZoneFile
compToCross (CompInputFile p) =
    CrossZoneFile $ flip replaceExtension (ext CrossZone) $ dropExtension p

compToMaskArrival :: CompInputFile -> MaskArrivalFile
compToMaskArrival (CompInputFile p) =
    MaskArrivalFile $ flip replaceExtension (ext MaskArrival) $ dropExtension p

compToMaskEffort :: CompInputFile -> MaskEffortFile
compToMaskEffort (CompInputFile p) =
    MaskEffortFile $ flip replaceExtension (ext MaskEffort) $ dropExtension p

compToMaskLead :: CompInputFile -> MaskLeadFile
compToMaskLead (CompInputFile p) =
    MaskLeadFile $ flip replaceExtension (ext MaskLead) $ dropExtension p

compToMaskReach :: CompInputFile -> MaskReachFile
compToMaskReach (CompInputFile p) =
    MaskReachFile $ flip replaceExtension (ext MaskReach) $ dropExtension p

compToMaskSpeed :: CompInputFile -> MaskSpeedFile
compToMaskSpeed (CompInputFile p) =
    MaskSpeedFile $ flip replaceExtension (ext MaskSpeed) $ dropExtension p

compToBonusReach :: CompInputFile -> BonusReachFile
compToBonusReach (CompInputFile p) =
    BonusReachFile $ flip replaceExtension (ext BonusReach) $ dropExtension p

compToLand :: CompInputFile -> LandOutFile
compToLand (CompInputFile p) =
    LandOutFile $ flip replaceExtension (ext LandOut) $ dropExtension p

compToPoint :: CompInputFile -> GapPointFile
compToPoint (CompInputFile p) =
    GapPointFile $ flip replaceExtension (ext GapPoint) $ dropExtension p

crossToTag :: CrossZoneFile -> TagZoneFile
crossToTag (CrossZoneFile p) =
    TagZoneFile $ flip replaceExtension (ext TagZone) $ dropExtension p

tagToPeg :: TagZoneFile -> PegFrameFile
tagToPeg (TagZoneFile p) =
    PegFrameFile $ flip replaceExtension (ext PegFrame) $ dropExtension p

pilotPath :: Pilot -> FilePath
pilotPath (Pilot (PilotId k, PilotName s)) =
    s ++ " " ++ k

unpackTrackPath :: CompDir -> Int -> Pilot -> (UnpackTrackDir, UnpackTrackFile)
unpackTrackPath dir task pilot =
    (unpackTrackDir dir task, UnpackTrackFile $ pilotPath pilot <.> "csv")

alignTimePath :: CompDir -> Int -> Pilot -> (AlignTimeDir, AlignTimeFile)
alignTimePath dir task pilot =
    (alignTimeDir dir task, AlignTimeFile $ pilotPath pilot <.> "csv")

discardFurtherPath :: CompDir -> Int -> Pilot -> (DiscardFurtherDir, DiscardFurtherFile)
discardFurtherPath dir task pilot =
    (discardFurtherDir dir task, DiscardFurtherFile $ pilotPath pilot <.> "csv")

unpackTrackDir :: CompDir -> Int -> UnpackTrackDir
unpackTrackDir comp task =
    UnpackTrackDir $ dotDir comp "unpack-track" task

alignTimeDir :: CompDir -> Int -> AlignTimeDir
alignTimeDir comp task =
    AlignTimeDir $ dotDir comp "align-time" task

discardFurtherDir :: CompDir -> Int -> DiscardFurtherDir
discardFurtherDir comp task =
    DiscardFurtherDir $ dotDir comp "discard-further" task

pegThenDiscardDir :: CompDir -> Int -> PegThenDiscardDir
pegThenDiscardDir comp task =
    PegThenDiscardDir $ dotDir comp "peg-then-discard" task

dotDir :: CompDir -> FilePath -> Int -> FilePath
dotDir (CompDir dir) name task =
    dir </> ".flare-timing" </> name </> "task-" ++ show task

data FileType
    = Fsdb
    | Kml
    | Igc
    | CompInput
    | NormScore
    | TaskLength
    | CrossZone
    | TagZone
    | PegFrame
    | UnpackTrack
    | AlignTime
    | DiscardFurther
    | PegThenDiscard
    | MaskArrival
    | MaskEffort
    | MaskLead
    | MaskReach
    | MaskSpeed
    | BonusReach
    | LandOut
    | GapPoint

ext :: FileType -> FilePath
ext Fsdb = ".fsdb"
ext Kml = ".kml"
ext Igc = ".igc"
ext CompInput = ".comp-input.yaml"
ext NormScore = ".norm-score.yaml"
ext TaskLength = ".task-length.yaml"
ext CrossZone = ".cross-zone.yaml"
ext TagZone = ".tag-zone.yaml"
ext PegFrame = ".peg-frame.yaml"
ext UnpackTrack = ".unpack-track.csv"
ext AlignTime = ".align-time.csv"
ext DiscardFurther = ".discard-further.csv"
ext PegThenDiscard = ".peg-then-discard.csv"
ext MaskArrival = ".mask-arrival.yaml"
ext MaskEffort = ".mask-effort.yaml"
ext MaskLead = ".mask-lead.yaml"
ext MaskReach = ".mask-reach.yaml"
ext MaskSpeed = ".mask-speed.yaml"
ext BonusReach = ".bonus-reach.yaml"
ext LandOut = ".land-out.yaml"
ext GapPoint = ".gap-point.yaml"

ensureExt :: FileType -> FilePath -> FilePath
ensureExt Fsdb = flip replaceExtensions "fsdb"
ensureExt Kml = id
ensureExt Igc = id
ensureExt CompInput = flip replaceExtensions "comp-input.yaml"
ensureExt NormScore = flip replaceExtensions "norm-score.yaml"
ensureExt TaskLength = flip replaceExtensions "task-length.yaml"
ensureExt CrossZone = flip replaceExtensions "cross-zone.yaml"
ensureExt TagZone = flip replaceExtensions "tag-zone.yaml"
ensureExt PegFrame = flip replaceExtensions "peg-frame.yaml"
ensureExt UnpackTrack = flip replaceExtensions "unpack-track.csv"
ensureExt AlignTime = flip replaceExtensions "align-time.csv"
ensureExt DiscardFurther = flip replaceExtensions "discard-further.csv"
ensureExt PegThenDiscard = flip replaceExtensions "peg-then-discard.csv"
ensureExt MaskArrival = flip replaceExtensions "mask-arrival.yaml"
ensureExt MaskEffort = flip replaceExtensions "mask-effort.yaml"
ensureExt MaskLead = flip replaceExtensions "mask-lead.yaml"
ensureExt MaskReach = flip replaceExtensions "mask-reach.yaml"
ensureExt MaskSpeed = flip replaceExtensions "mask-speed.yaml"
ensureExt BonusReach = flip replaceExtensions "bonus-reach.yaml"
ensureExt LandOut = flip replaceExtensions "land-out.yaml"
ensureExt GapPoint = flip replaceExtensions "gap-point.yaml"

findFsdb' :: FilePath -> IO [FsdbFile]
findFsdb' dir = fmap FsdbFile <$> findFiles Fsdb dir

findCompInput' :: FilePath -> IO [CompInputFile]
findCompInput' dir = fmap CompInputFile <$> findFiles CompInput dir

findNormScore' :: FilePath -> IO [NormScoreFile]
findNormScore' dir = fmap NormScoreFile <$> findFiles NormScore dir

findCrossZone' :: FilePath -> IO [CrossZoneFile]
findCrossZone' dir = fmap CrossZoneFile <$> findFiles CrossZone dir

findIgc' :: FilePath -> IO [IgcFile]
findIgc' dir = fmap IgcFile <$> findFiles Igc dir

findKml' :: FilePath -> IO [KmlFile]
findKml' dir = fmap KmlFile <$> findFiles Kml dir

findFiles :: FileType -> FilePath -> IO [FilePath]
findFiles typ =
    find always (fileType ==? Find.RegularFile &&? extension ==? ext typ)

findFsdb
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [FsdbFile]
findFsdb = findFileType Fsdb findFsdb' FsdbFile

findCompInput
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [CompInputFile]
findCompInput = findFileType CompInput findCompInput' CompInputFile

findNormScore
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [NormScoreFile]
findNormScore = findFileType NormScore findNormScore' NormScoreFile

findCrossZone
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [CrossZoneFile]
findCrossZone = findFileType CrossZone findCrossZone' CrossZoneFile

findIgc
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [IgcFile]
findIgc = findFileType Igc findIgc' IgcFile

findKml
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [KmlFile]
findKml = findFileType Kml findKml' KmlFile

findFileType
    :: (HasField "dir" o FilePath, HasField "file" o FilePath)
    => FileType
    -> (FilePath -> IO [a])
    -> (FilePath -> a)
    -> o
    -> IO [a]
findFileType typ finder ctor o = do
    dfe <- doesFileExist file
    if dfe then return [ctor file] else do
        dde <- doesDirectoryExist dir
        if dde then finder dir else return []
    where
        dir = getField @"dir" o
        file = ensureExt typ $ getField @"file" o
