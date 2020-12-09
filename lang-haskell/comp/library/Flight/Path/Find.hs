module Flight.Path.Find
    ( FindDirFile(..)
    , findNormArrival
    , findNormLandout
    , findNormRoute
    , findNormScore
    , findFsdb
    , findCleanFsdb
    , findTrimFsdb
    , findCompInput
    , findCrossZone
    , findIgc
    , findKml
    ) where

import GHC.Records
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), FilePath)
import System.FilePath.Find
    ((==?), (&&?), find, always, fileType, extension)
import qualified System.FilePath.Find as Find (FileType(..))
import Flight.Path.Types
import Flight.Path.Tx (reshape)

data FindDirFile =
    FindDirFile
        { dir :: FilePath
        , file :: FilePath
        }
    deriving Show

findNormArrival' :: FilePath -> IO [NormArrivalFile]
findNormArrival' dir = fmap NormArrivalFile <$> findFiles DotFs NormArrival dir

findNormArrival
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [NormArrivalFile]
findNormArrival = findFileType NormArrival findNormArrival' NormArrivalFile

findNormLandout' :: FilePath -> IO [NormLandoutFile]
findNormLandout' dir = fmap NormLandoutFile <$> findFiles DotFs NormLandout dir

findNormLandout
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [NormLandoutFile]
findNormLandout = findFileType NormLandout findNormLandout' NormLandoutFile

findNormRoute' :: FilePath -> IO [NormRouteFile]
findNormRoute' dir = fmap NormRouteFile <$> findFiles DotFs NormRoute dir

findNormRoute
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [NormRouteFile]
findNormRoute = findFileType NormRoute findNormRoute' NormRouteFile

findNormScore' :: FilePath -> IO [NormScoreFile]
findNormScore' dir = fmap NormScoreFile <$> findFiles DotFs NormScore dir

findNormScore
    :: (HasField "dir" o String, HasField "file" o String)
    => o
    -> IO [NormScoreFile]
findNormScore = findFileType NormScore findNormScore' NormScoreFile

findFsdb' :: FilePath -> IO [FsdbFile]
findFsdb' dir = fmap FsdbFile <$> findFiles DotRoot Fsdb dir

findCleanFsdb' :: FilePath -> IO [CleanFsdbFile]
findCleanFsdb' dir = fmap CleanFsdbFile <$> findFiles DotFt Fsdb dir

findTrimFsdb' :: FilePath -> IO [TrimFsdbFile]
findTrimFsdb' dir = fmap TrimFsdbFile <$> findFiles DotFs TrimFsdb (dir </> ".flight-system")

findCompInput' :: FilePath -> IO [CompInputFile]
findCompInput' dir = fmap CompInputFile <$> findFiles DotFt CompInput dir

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

ext :: FileType -> FilePath
ext Fsdb = ".fsdb"
ext CleanFsdb = ".clean-fsdb.xml"
ext TrimFsdb = ".trim-fsdb.xml"
ext Kml = ".kml"
ext Igc = ".igc"
ext NormArrival = ".norm-arrival.yaml"
ext NormLandout = ".norm-land-out.yaml"
ext NormRoute = ".norm-route.yaml"
ext NormScore = ".norm-score.yaml"
ext CompInput = ".comp-input.yaml"
ext TaskLength = ".task-length.yaml"
ext CrossZone = ".cross-zone.yaml"
ext TagZone = ".tag-zone.yaml"
ext PegFrame = ".peg-frame.yaml"
ext UnpackTrack = ".unpack-track.csv"
ext AlignTime = ".align-time.csv"
ext DiscardFurther = ".discard-further.csv"
ext PegThenDiscard = ".peg-then-discard.csv"
ext AreaStep = ".area-step.csv"
ext LeadArea = ".lead-area.yaml"
ext MaskArrival = ".mask-arrival.yaml"
ext MaskEffort = ".mask-effort.yaml"
ext MaskLead = ".mask-lead.yaml"
ext MaskReach = ".mask-reach.yaml"
ext MaskSpeed = ".mask-speed.yaml"
ext BonusReach = ".bonus-reach.yaml"
ext LandOut = ".land-out.yaml"
ext FarOut = ".far-out.yaml"
ext GapPoint = ".gap-point.yaml"
