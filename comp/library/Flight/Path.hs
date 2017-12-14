module Flight.Path
    ( FsdbFile(..)
    , FsdbXml(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , CompDir(..)
    , AlignDir(..)
    , DiscardDir(..)
    , fsdbToComp
    , compToTaskLength
    , compToCross
    , crossToTag
    , compFileToCompDir
    , alignDir
    , discardDir
    , alignPath
    , discardPath
    ) where

import System.FilePath
    ( FilePath
    , (</>), (<.>)
    , replaceExtension, dropExtension, takeDirectory
    )
import Flight.Pilot (Pilot(..))

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

fsdbToComp :: FsdbFile -> CompInputFile
fsdbToComp (FsdbFile p) =
    CompInputFile $ replaceExtension p ".comp-input.yaml"

compFileToCompDir :: CompInputFile -> CompDir
compFileToCompDir (CompInputFile p) =
    CompDir $ takeDirectory p

compToTaskLength :: CompInputFile -> TaskLengthFile
compToTaskLength (CompInputFile p) =
    TaskLengthFile $ flip replaceExtension ".task-length.yaml" $ dropExtension p

compToCross :: CompInputFile -> CrossZoneFile
compToCross (CompInputFile p) =
    CrossZoneFile $ flip replaceExtension ".cross-zone.yaml" $ dropExtension p

crossToTag :: CrossZoneFile -> TagZoneFile
crossToTag (CrossZoneFile p) =
    TagZoneFile $ flip replaceExtension ".tag-zone.yaml" $ dropExtension p

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
