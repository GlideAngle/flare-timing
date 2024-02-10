module Flight.Path.Find
    ( FindDirFile(..)
    , findAltArrival
    , findAltLandout
    , findAltRoute
    , findAltScore
    , findFsdb
    , findCleanFsdb
    , findTrimFsdb
    , findCompInput
    , findTaskInput
    , findFlyTime
    , findCrossZone
    , findIgc
    , findKml
    , compFileToTaskFiles
    ) where

import GHC.Records
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory)
import System.FilePath.Find
    ((==?), (&&?), find, always, fileType, extension, fileName)
import qualified System.FilePath.Find as Find (FileType(..))
import Flight.Path.Types
import Flight.Path.Tx (reshape)

data FindDirFile =
    FindDirFile
        { dir :: FilePath
        , file :: FilePath
        }
    deriving Show

compFileToTaskFiles :: CompInputFile -> IO [TaskInputFile]
compFileToTaskFiles (CompInputFile pathComp) = do
    let pathTask = reshape TaskInput pathComp
    findTaskInput $ FindDirFile {dir = takeDirectory pathComp, file = pathTask}

findAltArrival' :: AltDot -> FilePath -> IO [AltArrivalFile]
findAltArrival' AltFs dir = fmap AltArrivalFile <$> findFiles DotFs (AltArrival AltFs) dir
findAltArrival' AltAs dir = fmap AltArrivalFile <$> findFiles DotFs (AltArrival AltAs) dir

findAltArrival
    :: (HasField "dir" o String, HasField "file" o String)
    => AltDot
    -> o
    -> IO [AltArrivalFile]
findAltArrival a = findFileType (AltArrival a) (findAltArrival' a) AltArrivalFile

findAltLandout' :: AltDot -> FilePath -> IO [AltLandoutFile]
findAltLandout' AltFs dir = fmap AltLandoutFile <$> findFiles DotFs (AltLandout AltFs) dir
findAltLandout' AltAs dir = fmap AltLandoutFile <$> findFiles DotAs (AltLandout AltAs) dir

findAltLandout
    :: (HasField "dir" o String, HasField "file" o String)
    => AltDot
    -> o
    -> IO [AltLandoutFile]
findAltLandout a = findFileType (AltLandout a) (findAltLandout' a) AltLandoutFile

findAltRoute' :: AltDot -> FilePath -> IO [AltRouteFile]
findAltRoute' AltFs dir = fmap AltRouteFile <$> findFiles DotFs (AltRoute AltFs) dir
findAltRoute' AltAs dir = fmap AltRouteFile <$> findFiles DotAs (AltRoute AltAs) dir

findAltRoute
    :: (HasField "dir" o String, HasField "file" o String)
    => AltDot
    -> o
    -> IO [AltRouteFile]
findAltRoute a = findFileType (AltRoute a) (findAltRoute' a) AltRouteFile

findAltScore' :: AltDot -> FilePath -> IO [AltScoreFile]
findAltScore' AltFs dir = fmap AltScoreFile <$> findFiles DotFs (AltScore AltFs) dir
findAltScore' AltAs dir = fmap AltScoreFile <$> findFiles DotAs (AltScore AltAs) dir

findAltScore
    :: (HasField "dir" o String, HasField "file" o String)
    => AltDot
    -> o
    -> IO [AltScoreFile]
findAltScore a = findFileType (AltScore a) (findAltScore' a) AltScoreFile

findFsdb' :: FilePath -> IO [FsdbFile]
findFsdb' dir = fmap FsdbFile <$> findFiles DotRoot Fsdb dir

findCleanFsdb' :: FilePath -> IO [CleanFsdbFile]
findCleanFsdb' dir = fmap CleanFsdbFile <$> findFiles DotFt Fsdb dir

findTrimFsdb' :: FilePath -> IO [TrimFsdbFile]
findTrimFsdb' dir = fmap TrimFsdbFile <$> findFiles DotFs TrimFsdb (dir </> ".flight-system")

findCompInput' :: FilePath -> IO [CompInputFile]
findCompInput' dir = fmap CompInputFile <$> findFiles DotFt CompInput dir

findFlyTime' :: FilePath -> IO [FlyTimeFile]
findFlyTime' dir = fmap FlyTimeFile <$> findFiles DotFt FlyTime dir

findCrossZone' :: FilePath -> IO [CrossZoneFile]
findCrossZone' dir = fmap CrossZoneFile <$> findFiles DotFt CrossZone dir

findIgc' :: FilePath -> IO [IgcFile]
findIgc' dir = fmap IgcFile <$> findFiles DotRoot Igc dir

findKml' :: FilePath -> IO [KmlFile]
findKml' dir = fmap KmlFile <$> findFiles DotRoot Kml dir

findFiles :: DotFolder -> FileType -> FilePath -> IO [FilePath]
findFiles dotFolder typ
    | DotRoot <- dotFolder =
        find always (fileType ==? Find.RegularFile &&? extension ==? ext typ)
    | DotFt <- dotFolder =
        find always (fileType ==? Find.RegularFile &&? extension ==? ext typ)
    | DotFs <- dotFolder =
        find always (fileType ==? Find.RegularFile &&? extension ==? ext typ)
    | DotAs <- dotFolder =
        find always (fileType ==? Find.RegularFile &&? extension ==? ext typ)

findFsdb
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [FsdbFile]
findFsdb = findFileType Fsdb findFsdb' FsdbFile

findCleanFsdb
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [CleanFsdbFile]
findCleanFsdb = findFileType CleanFsdb findCleanFsdb' CleanFsdbFile

findTrimFsdb
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [TrimFsdbFile]
findTrimFsdb = findFileType TrimFsdb findTrimFsdb' TrimFsdbFile

findCompInput
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [CompInputFile]
findCompInput = findFileType CompInput findCompInput' CompInputFile

findTaskInput
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [TaskInputFile]
findTaskInput o =
    fmap TaskInputFile <$> find always (fileName ==? file) dir
    where
        dir = getField @"dir" o
        file = getField @"file" o

findFlyTime
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [FlyTimeFile]
findFlyTime = findFileType FlyTime findFlyTime' FlyTimeFile

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
        file = reshape typ $ getField @"file" o

-- TODO: Figure out what to do with this function now that the extension or
-- compound extension does not by itself distinguish the file.
ext :: FileType -> FilePath

ext Fsdb = ".fsdb"
ext CleanFsdb = ".clean-fsdb.xml"
ext TrimFsdb = ".trim-fsdb.xml"

ext Kml = ".kml"
ext Igc = ".igc"

ext CompInput = ".comp-input.yaml"
ext TaskInput = ".task-input.yaml"
ext TaskLength = ".task-length.yaml"
ext FlyTime = ".fly-time.yaml"
ext CrossZone = ".cross-zone.yaml"
ext TagZone = ".tag-zone.yaml"
ext PegFrame = ".peg-frame.yaml"
ext LeadArea = ".lead-area.yaml"
ext MaskArrival = ".mask-arrival.yaml"
ext MaskEffort = ".mask-effort.yaml"
ext MaskLead = ".mask-lead.yaml"
ext MaskReach = ".mask-reach.yaml"
ext MaskSpeed = ".mask-speed.yaml"
ext MaskBonus = ".mask-bonus.yaml"
ext LandOut = ".land-out.yaml"
ext FarOut = ".far-out.yaml"
ext GapPoint = ".gap-point.yaml"

ext UnpackTrack = ".unpack-track.csv"
ext AlignTime = ".align-time.csv"
ext DiscardFurther = ".discard-further.csv"
ext PegThenDiscard = ".peg-then-discard.csv"
ext AreaStep = ".area-step.csv"

ext (AltArrival _) = "arrival.yaml"
ext (AltLandout _) = ".norm-land-out.yaml"
ext (AltRoute _) = ".norm-route.yaml"
ext (AltScore _) = ".norm-score.yaml"
