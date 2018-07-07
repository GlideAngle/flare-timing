module Flight.Pilot
    ( PilotId(..)
    , PilotName(..)
    , Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TaskFolder(..)
    , TrackFileFail(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

newtype PilotId =
    PilotId String 
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PilotName =
    PilotName String
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Pilot = Pilot (PilotId, PilotName)
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Order by name then by id.
instance Ord Pilot where
    (Pilot (k0, s0)) `compare` (Pilot (k1, s1)) =
        (s0, k0) `compare` (s1, k1)

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
