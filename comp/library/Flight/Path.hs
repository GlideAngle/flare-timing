module Flight.Path
    ( FileType(..)
    , IgcFile(..)
    , KmlFile(..)
    , FsdbFile(..)
    , FsdbXml(..)
    , CompInputFile(..)
    , ExpectedScoreFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , UnpackTrackFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , MaskTrackFile(..)
    , LandOutFile(..)
    , GapPointFile(..)
    , CompDir(..)
    , UnpackTrackDir(..)
    , AlignTimeDir(..)
    , DiscardFurtherDir(..)
    , fsdbToComp
    , fsdbToScore
    , compToTaskLength
    , compToCross
    , compToMask
    , compToLand
    , compToPoint
    , crossToTag
    , compFileToCompDir
    , unpackTrackDir
    , alignTimeDir
    , discardFurtherDir
    , unpackTrackPath
    , alignTimePath
    , discardFurtherPath
    , findFsdb
    , findCompInput
    , findExpectedScore
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
newtype ExpectedScoreFile = ExpectedScoreFile FilePath

-- | The path to a task length file.
newtype TaskLengthFile = TaskLengthFile FilePath

-- | The path to a cross zone file.
newtype CrossZoneFile = CrossZoneFile FilePath

-- | The path to a tag zone file.
newtype TagZoneFile = TagZoneFile FilePath

-- | The path to as unpack track directory for a single task.
newtype UnpackTrackDir = UnpackTrackDir FilePath

-- | The path to as align time directory for a single task.
newtype AlignTimeDir = AlignTimeDir FilePath

-- | The path to as discard further directory for a single task.
newtype DiscardFurtherDir = DiscardFurtherDir FilePath

-- | The path to as unpack track file.
newtype UnpackTrackFile = UnpackTrackFile FilePath

-- | The path to as align time file.
newtype AlignTimeFile = AlignTimeFile FilePath

-- | The path to a discard further file.
newtype DiscardFurtherFile = DiscardFurtherFile FilePath

-- | The path to as mask track file.
newtype MaskTrackFile = MaskTrackFile FilePath

-- | The path to as land out file.
newtype LandOutFile = LandOutFile FilePath

-- | The path to as gap point file.
newtype GapPointFile = GapPointFile FilePath

fsdbToComp :: FsdbFile -> CompInputFile
fsdbToComp (FsdbFile p) =
    CompInputFile $ replaceExtension p (ext CompInput)

fsdbToScore :: FsdbFile -> ExpectedScoreFile
fsdbToScore (FsdbFile p) =
    ExpectedScoreFile $ replaceExtension p (ext ExpectedScore)

compFileToCompDir :: CompInputFile -> CompDir
compFileToCompDir (CompInputFile p) =
    CompDir $ takeDirectory p

compToTaskLength :: CompInputFile -> TaskLengthFile
compToTaskLength (CompInputFile p) =
    TaskLengthFile $ flip replaceExtension (ext TaskLength) $ dropExtension p

compToCross :: CompInputFile -> CrossZoneFile
compToCross (CompInputFile p) =
    CrossZoneFile $ flip replaceExtension (ext CrossZone) $ dropExtension p

compToMask :: CompInputFile -> MaskTrackFile
compToMask (CompInputFile p) =
    MaskTrackFile $ flip replaceExtension (ext MaskTrack) $ dropExtension p

compToLand :: CompInputFile -> LandOutFile
compToLand (CompInputFile p) =
    LandOutFile $ flip replaceExtension (ext LandOut) $ dropExtension p

compToPoint :: CompInputFile -> GapPointFile
compToPoint (CompInputFile p) =
    GapPointFile $ flip replaceExtension (ext GapPoint) $ dropExtension p

crossToTag :: CrossZoneFile -> TagZoneFile
crossToTag (CrossZoneFile p) =
    TagZoneFile $ flip replaceExtension (ext TagZone) $ dropExtension p

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

dotDir :: CompDir -> FilePath -> Int -> FilePath
dotDir (CompDir dir) name task =
    dir </> ".flare-timing" </> name </> "task-" ++ show task

data FileType
    = Fsdb
    | Kml
    | Igc
    | CompInput
    | ExpectedScore
    | TaskLength
    | CrossZone
    | TagZone
    | UnpackTrack
    | AlignTime
    | DiscardFurther
    | MaskTrack
    | LandOut
    | GapPoint

ext :: FileType -> FilePath
ext Fsdb = ".fsdb"
ext Kml = ".kml"
ext Igc = ".igc"
ext CompInput = ".comp-input.yaml"
ext ExpectedScore = ".expected-score.yaml"
ext TaskLength = ".task-length.yaml"
ext CrossZone = ".cross-zone.yaml"
ext TagZone = ".tag-zone.yaml"
ext UnpackTrack = ".unpack-track.csv"
ext AlignTime = ".align-time.csv"
ext DiscardFurther = ".discard-further.csv"
ext MaskTrack = ".mask-track.yaml"
ext LandOut = ".land-out.yaml"
ext GapPoint = ".gap-point.yaml"

ensureExt :: FileType -> FilePath -> FilePath
ensureExt Fsdb = flip replaceExtensions "fsdb"
ensureExt Kml = id
ensureExt Igc = id
ensureExt CompInput = flip replaceExtensions "comp-input.yaml"
ensureExt ExpectedScore = flip replaceExtensions "expected-score.yaml"
ensureExt TaskLength = flip replaceExtensions "task-length.yaml"
ensureExt CrossZone = flip replaceExtensions "cross-zone.yaml"
ensureExt TagZone = flip replaceExtensions "tag-zone.yaml"
ensureExt UnpackTrack = flip replaceExtensions "unpack-track.csv"
ensureExt AlignTime = flip replaceExtensions "align-time.csv"
ensureExt DiscardFurther = flip replaceExtensions "discard-further.csv"
ensureExt MaskTrack = flip replaceExtensions "mask-track.yaml"
ensureExt LandOut = flip replaceExtensions "land-out.yaml"
ensureExt GapPoint = flip replaceExtensions "gap-point.yaml"

findFsdb' :: FilePath -> IO [FsdbFile]
findFsdb' dir = fmap FsdbFile <$> findFiles Fsdb dir

findCompInput' :: FilePath -> IO [CompInputFile]
findCompInput' dir = fmap CompInputFile <$> findFiles CompInput dir

findExpectedScore' :: FilePath -> IO [ExpectedScoreFile]
findExpectedScore' dir = fmap ExpectedScoreFile <$> findFiles ExpectedScore dir

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

findExpectedScore
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [ExpectedScoreFile]
findExpectedScore = findFileType ExpectedScore findExpectedScore' ExpectedScoreFile

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
