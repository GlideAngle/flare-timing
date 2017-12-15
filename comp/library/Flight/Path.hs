{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Flight.Path
    ( IgcFile(..)
    , KmlFile(..)
    , FsdbFile(..)
    , FsdbXml(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , MaskTrackFile(..)
    , CompDir(..)
    , AlignDir(..)
    , DiscardDir(..)
    , fsdbToComp
    , compToTaskLength
    , compToCross
    , compToMask
    , crossToTag
    , compFileToCompDir
    , alignDir
    , discardDir
    , alignPath
    , discardPath
    , findFsdb
    , findCompInput
    , findCrossZone
    , findIgc
    , findKml
    ) where

import GHC.Records
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath
    (FilePath, (</>), (<.>), replaceExtension, dropExtension, takeDirectory)
import System.FilePath.Find
    ((==?), (&&?), find, always, fileType, extension)
import qualified System.FilePath.Find as Find (FileType(..))
import Flight.Pilot (Pilot(..))

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

-- | The path to a task length file.
newtype TaskLengthFile = TaskLengthFile FilePath

-- | The path to a cross zone file.
newtype CrossZoneFile = CrossZoneFile FilePath

-- | The path to a tag zone file.
newtype TagZoneFile = TagZoneFile FilePath

-- | The path to as align time directory for a single task.
newtype AlignDir = AlignDir FilePath

-- | The path to as discard further directory for a single task.
newtype DiscardDir = DiscardDir FilePath

-- | The path to as align time file.
newtype AlignTimeFile = AlignTimeFile FilePath

-- | The path to a discard further file.
newtype DiscardFurtherFile = DiscardFurtherFile FilePath

-- | The path to as mask track file.
newtype MaskTrackFile = MaskTrackFile FilePath

fsdbToComp :: FsdbFile -> CompInputFile
fsdbToComp (FsdbFile p) =
    CompInputFile $ replaceExtension p (ext CompInput)

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

crossToTag :: CrossZoneFile -> TagZoneFile
crossToTag (CrossZoneFile p) =
    TagZoneFile $ flip replaceExtension (ext TagZone) $ dropExtension p

alignPath :: CompDir -> Int -> Pilot -> (AlignDir, AlignTimeFile)
alignPath dir task pilot =
    (alignDir dir task, AlignTimeFile $ show pilot <.> "csv")

discardPath :: CompDir -> Int -> Pilot -> (DiscardDir, DiscardFurtherFile)
discardPath dir task pilot =
    (discardDir dir task, DiscardFurtherFile $ show pilot <.> "csv")

alignDir :: CompDir -> Int -> AlignDir
alignDir comp task =
    AlignDir $ dotDir comp "align-time" task

discardDir :: CompDir -> Int -> DiscardDir
discardDir comp task =
    DiscardDir $ dotDir comp "discard-further" task

dotDir :: CompDir -> FilePath -> Int -> FilePath
dotDir (CompDir dir) name task =
    dir </> ".flare-timing" </> name </> "task-" ++ show task

data FileType
    = Fsdb
    | Kml
    | Igc
    | CompInput
    | TaskLength
    | CrossZone
    | TagZone
    | AlignTime
    | DiscardFurther
    | MaskTrack

ext :: FileType -> FilePath
ext Fsdb = ".fsdb"
ext Kml = ".kml"
ext Igc = ".igc"
ext CompInput = ".comp-input.yaml"
ext TaskLength = ".task-length.yaml"
ext CrossZone = ".cross-zone.yaml"
ext TagZone = ".tag-zone.yaml"
ext AlignTime = ".align-time.yaml"
ext DiscardFurther = ".discard-further.yaml"
ext MaskTrack = ".mask-track.yaml"

findFsdb' :: FilePath -> IO [FsdbFile]
findFsdb' dir = findFiles Fsdb dir >>= return . fmap FsdbFile

findCompInput' :: FilePath -> IO [CompInputFile]
findCompInput' dir = findFiles CompInput dir >>= return . fmap CompInputFile

findCrossZone' :: FilePath -> IO [CrossZoneFile]
findCrossZone' dir = findFiles CrossZone dir >>= return . fmap CrossZoneFile

findIgc' :: FilePath -> IO [IgcFile]
findIgc' dir = findFiles Igc dir >>= return . fmap IgcFile

findKml' :: FilePath -> IO [KmlFile]
findKml' dir = findFiles Kml dir >>= return . fmap KmlFile

findFiles :: FileType -> FilePath -> IO [FilePath]
findFiles typ dir =
    find always (fileType ==? Find.RegularFile &&? extension ==? ext typ) dir

findFsdb :: (HasField "dir" o String , HasField "file" o String)
         => o
         -> IO [FsdbFile]
findFsdb = findFileType findFsdb' FsdbFile

findCompInput :: (HasField "dir" o String , HasField "file" o String)
              => o
              -> IO [CompInputFile]
findCompInput = findFileType findCompInput' CompInputFile

findCrossZone :: (HasField "dir" o String , HasField "file" o String)
              => o
              -> IO [CrossZoneFile]
findCrossZone = findFileType findCrossZone' CrossZoneFile

findIgc :: (HasField "dir" o String , HasField "file" o String)
        => o
        -> IO [IgcFile]
findIgc = findFileType findIgc' IgcFile

findKml :: (HasField "dir" o String , HasField "file" o String)
        => o
        -> IO [KmlFile]
findKml = findFileType findKml' KmlFile

findFileType :: (HasField "dir" o FilePath, HasField "file" o FilePath)
             => (FilePath -> IO [a])
             -> (FilePath -> a)
             -> o
             -> IO [a]
findFileType finder ctor o = do
    dfe <- doesFileExist file
    if dfe then return [ctor file] else do
        dde <- doesDirectoryExist dir
        if dde then finder dir else return []
    where
        dir = getField @"dir" o
        file = getField @"file" o
