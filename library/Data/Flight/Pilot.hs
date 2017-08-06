{-# LANGUAGE DeriveGeneric #-}

module Data.Flight.Pilot
    ( Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TaskFolder(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

newtype Pilot = Pilot String deriving (Eq, Ord, Generic)
newtype TrackLogFile = TrackLogFile String deriving (Eq, Ord, Generic)

-- | A task folder is relative. To be cross-platform I store the path
-- parts, stripping the path separators.
newtype TaskFolder = TaskFolder [ String ] deriving (Generic)


data PilotTrackLogFile =
    PilotTrackLogFile Pilot (Maybe TrackLogFile) deriving (Eq, Ord, Generic)

instance ToJSON TaskFolder
instance FromJSON TaskFolder

instance ToJSON Pilot
instance FromJSON Pilot

instance ToJSON TrackLogFile
instance FromJSON TrackLogFile

instance ToJSON PilotTrackLogFile
instance FromJSON PilotTrackLogFile

instance Show Pilot where
    show (Pilot name) = name

instance Show TrackLogFile where
    show (TrackLogFile name) = name

instance Show TaskFolder where
    show (TaskFolder pathParts) = show pathParts

instance Show PilotTrackLogFile where
    show (PilotTrackLogFile pilot Nothing) = show pilot ++ " -"
    show (PilotTrackLogFile pilot (Just tlf)) = show pilot ++ " <<" ++ show tlf ++ ">>"
