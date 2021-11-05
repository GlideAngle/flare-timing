module Flight.Pilot
    ( PilotTaskStatus(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TaskFolder(..)
    , TrackFileFail(..)
    , Dnf(..)
    , Nyp(..)
    , DfNoTrackPilot(..)
    , DfNoTrack(..)
    , LandedOut(..)
    , MadeGoal(..)
    , dfNoTrackReach
    ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), Options(..)
    , defaultOptions, genericToJSON, genericParseJSON
    )
import Data.UnitsOfMeasure ((*:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), QTaskDistance)
import "flight-gap-allot" Flight.Score (Pilot, LinearFraction(..))
import "flight-gap-valid" Flight.Score (ReachToggle(..))
import Flight.Track.Time (AwardedVelocity)
import Flight.Track.Distance (AwardedDistance(..), TrackReach(..))

dfNoTrackReach
    :: QTaskDistance Double [u| km |]
    -> DfNoTrackPilot
    -> (Pilot, ReachToggle TrackReach)
dfNoTrackReach (TaskDistance td) DfNoTrackPilot{pilot, awardedReach} =
    (pilot,) $
    maybe
        (let r = TrackReach (TaskDistance [u| 0 m |]) (LinearFraction 0) in ReachToggle r r)
        (\ReachToggle
            { flown = AwardedDistance{awardedFrac = aF}
            , extra = AwardedDistance{awardedFrac = aE}
            } ->
            ReachToggle
                { flown =
                    TrackReach
                        (TaskDistance $ MkQuantity aF *: td)
                        (LinearFraction $ toRational aF)
                , extra =
                    TrackReach
                        (TaskDistance $ MkQuantity aE *: td)
                        (LinearFraction $ toRational aE)
                })
        awardedReach

-- | The group of pilots that did not fly a task.
newtype Dnf = Dnf {unDnf :: [Pilot]}

-- | The group of pilots not yet processed.
newtype Nyp = Nyp {unNyp :: [Pilot]}

data DfNoTrackPilot =
    DfNoTrackPilot
        { pilot :: Pilot
        , awardedReach :: Maybe (ReachToggle AwardedDistance)
        , awardedVelocity :: AwardedVelocity
        }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | The group of pilots that flew but did not submit a tracklog.
newtype DfNoTrack =
    DfNoTrack
        {unDfNoTrack :: [DfNoTrackPilot]}
    deriving (Eq, Ord, Show, Generic)

instance ToJSON DfNoTrack where
    toJSON = genericToJSON defaultOptions{unwrapUnaryRecords = True}

instance FromJSON DfNoTrack where
    parseJSON = genericParseJSON defaultOptions{unwrapUnaryRecords = True}

-- | The group of pilots that landed out on course.
newtype LandedOut = LandedOut {unLandedOut :: [Pilot]}

-- | The group of pilots that made goal.
newtype MadeGoal = MadeGoal {unMadeGoal :: [Pilot]}

data PilotTaskStatus
    = ABS -- ^ Absent
    | DF -- ^ Did fly
    | DFNoTrack -- ^ Did fly but with no tracklog for scoring.
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
    deriving (Eq, Ord, Generic, ToJSON, FromJSON, NFData)

instance Show TrackFileFail where
    show (TaskFolderExistsNot x) = "Folder '" ++ x ++ "' not found"
    show (TrackLogFileExistsNot x) = "File '" ++ x ++ "' not found"
    show TrackLogFileNotSet = "File not set"
    show (TrackLogFileNotRead "") = "File not read"
    show (TrackLogFileNotRead x) = "File not read " ++ x
