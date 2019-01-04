module Flight.Pilot
    ( PilotTaskStatus(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TaskFolder(..)
    , TrackFileFail(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Score (Pilot)

data PilotTaskStatus
    = ABS -- ^ Absent
    | DF -- ^ Did fly
    | DNF -- ^ Did not fly
    | NYP -- ^ Not yet processed
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype TrackLogFile = TrackLogFile String
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Show TrackLogFile where
    show (TrackLogFile name) = name

-- | A task folder is relative. To be cross-platform I store the path
-- parts, stripping the path separators.
newtype TaskFolder = TaskFolder [ String ]
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Show TaskFolder where
    show (TaskFolder pathParts) = show pathParts

data PilotTrackLogFile = PilotTrackLogFile Pilot (Maybe TrackLogFile)
    deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance Show PilotTrackLogFile where
    show (PilotTrackLogFile pilot Nothing) =
        show pilot ++ " -"
    show (PilotTrackLogFile pilot (Just tlf)) =
        show pilot ++ " <<" ++ show tlf ++ ">>"

data TrackFileFail
    = TaskFolderExistsNot String
    | TrackLogFileExistsNot String
    | TrackLogFileNotSet
    | TrackLogFileNotRead String
    deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance Show TrackFileFail where
    show (TaskFolderExistsNot x) = "Folder '" ++ x ++ "' not found"
    show (TrackLogFileExistsNot x) = "File '" ++ x ++ "' not found"
    show TrackLogFileNotSet = "File not set"
    show (TrackLogFileNotRead "") = "File not read"
    show (TrackLogFileNotRead x) = "File not read " ++ x
