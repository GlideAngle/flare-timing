module Flight.Path
    ( FsdbFile(..)
    , FsdbXml(..)
    , CompFile(..)
    , TaskLengthFile(..)
    , CrossFile(..)
    , TagFile(..)
    , AlignFile(..)
    , DiscardFile(..)
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
newtype CompFile = CompFile FilePath

-- | The path to a task length file.
newtype TaskLengthFile = TaskLengthFile FilePath

-- | The path to a cross zone file.
newtype CrossFile = CrossFile FilePath

-- | The path to a tag zone file.
newtype TagFile = TagFile FilePath

-- | The path to as align time directory for a single task.
newtype AlignDir = AlignDir FilePath

-- | The path to as discard further directory for a single task.
newtype DiscardDir = DiscardDir FilePath

-- | The path to as align time file.
newtype AlignFile = AlignFile FilePath

-- | The path to a discard further file.
newtype DiscardFile = DiscardFile FilePath

fsdbToComp :: FsdbFile -> CompFile
fsdbToComp (FsdbFile p) =
    CompFile $ replaceExtension p ".comp-input.yaml"

compFileToCompDir :: CompFile -> CompDir
compFileToCompDir (CompFile p) =
    CompDir $ takeDirectory p

compToTaskLength :: CompFile -> TaskLengthFile
compToTaskLength (CompFile p) =
    TaskLengthFile $ flip replaceExtension ".task-length.yaml" $ dropExtension p

compToCross :: CompFile -> CrossFile
compToCross (CompFile p) =
    CrossFile $ flip replaceExtension ".cross-zone.yaml" $ dropExtension p

crossToTag :: CrossFile -> TagFile
crossToTag (CrossFile p) =
    TagFile $ flip replaceExtension ".tag-zone.yaml" $ dropExtension p

alignPath :: CompDir -> Int -> Pilot -> (AlignDir, AlignFile)
alignPath dir task pilot =
    (alignDir dir task, AlignFile $ show pilot <.> "csv")

discardPath :: CompDir -> Int -> Pilot -> (DiscardDir, DiscardFile)
discardPath dir task pilot =
    (discardDir dir task, DiscardFile $ show pilot <.> "csv")

alignDir :: CompDir -> Int -> AlignDir
alignDir comp task =
    AlignDir $ dotDir comp "align-time" task

discardDir :: CompDir -> Int -> DiscardDir
discardDir comp task =
    DiscardDir $ dotDir comp "discard-further" task

dotDir :: CompDir -> FilePath -> Int -> FilePath
dotDir (CompDir dir) name task =
    dir </> ".flare-timing" </> name </> "task-" ++ show task
