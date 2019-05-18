{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Data.Flight.Comp
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Data for competitions, competitors and tasks.
-}
module Flight.Comp
    ( -- * Competition
      CompSettings(..)
    , Projection(..)
    , EarthModel(..)
    , EarthMath(..)
    , Comp(..)
    , Nominal(..)
    , Tweak(..)
    , UtcOffset(..)
    , PilotGroup(..)
    , defaultNominal
    -- * Task
    , Task(..)
    , TaskStop(..)
    , IxTask(..)
    , Zones(..)
    , StartGate(..)
    , OpenClose(..)
    , FirstLead(..)
    , FirstStart(..)
    , LastStart(..)
    , LastArrival(..)
    , LastDown(..)
    , StartEnd(..)
    , StartEndMark
    , StartEndDown(..)
    , StartEndDownMark
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , showTask
    , openClose
    , speedSectionToLeg
    -- * Pilot and their track logs.
    , PilotId(..)
    , PilotName(..)
    , Pilot(..)
    , PilotTaskStatus(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TrackFileFail(..)
    , TaskFolder(..)
    , Dnf(..)
    , Nyp(..)
    , DfNoTrack(..)
    , DfNoTrackPilot(..)
    , LandedOut(..)
    , MadeGoal(..)
    , pilotNamed
    -- * Comp paths
    , module Flight.Path
    ) where

import Data.Ratio ((%))
import Control.Applicative (empty)
import Control.Monad (join)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson
    ( Value(..), ToJSON(..), FromJSON(..), Options(..), SumEncoding(..)
    , genericToJSON, genericParseJSON, defaultOptions
    )
import Data.Maybe (listToMaybe)
import Data.List (intercalate, nub, sort)
import Data.String (IsString())
import qualified Data.Text as T (pack)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (QRadius)
import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Zone.MkZones (Zones(..), Discipline(..))
import Flight.Zone.Raw (Give, showZone)
import Flight.Field (FieldOrdering(..))
import Flight.Pilot
import Flight.Path
import Flight.Distance (QTaskDistance)
import Flight.Score
    ( Leg(..)
    , NominalLaunch(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalTime(..)
    , ScoreBackTime(..)
    , PilotId(..)
    , PilotName(..)
    , Pilot(..)
    , LwScaling(..)
    , AwScaling(..)
    , PointPenalty(..)
    )
import Flight.Earth.Ellipsoid (Ellipsoid(..))

-- | The time of first lead into the speed section. This won't exist if no one
-- is able to cross the start of the speed section without bombing out.
newtype FirstLead = FirstLead UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The time of first start of the speed section. This won't exist if everyone
-- jumps the gun.
newtype FirstStart = FirstStart UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The time of last start of the speed section. This won't exist if everyone
-- jumps the gun.
newtype LastStart = LastStart UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The time of last crossing of the end of the speed section. This won't
-- exist if no one makes goal and everyone lands out.
newtype LastArrival = LastArrival UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The time of the last landing.
newtype LastDown = LastDown UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A race task can be started and not finished if no one makes goal.
data StartEnd a b =
    StartEnd
        { unStart :: a
        , unEnd :: Maybe b
        }
    deriving Show

data StartEndDown a b =
    StartEndDown
        { unStart :: a
        , unEnd :: Maybe b
        , unDown :: Maybe b
        }
    deriving Show

type StartEndMark = StartEnd UTCTime UTCTime
type StartEndDownMark = StartEndDown UTCTime UTCTime

-- | 1-based indices of a task in a competition.
newtype IxTask = IxTask Int deriving (Eq, Show)

speedSectionToLeg :: SpeedSection -> Int -> Leg
speedSectionToLeg Nothing i = RaceLeg i
speedSectionToLeg (Just (s, e)) i =
    if | i < s -> PrologLeg i
       | i > e -> EpilogLeg i
       | True -> RaceLeg i

type RoutesLookup a = IxTask -> Maybe a

data TaskRouteDistance =
    TaskRouteDistance
        { wholeTaskDistance :: QTaskDistance Double [u| m |]
        , speedSubsetDistance :: QTaskDistance Double [u| m |]
        , launchToEssDistance :: QTaskDistance Double [u| m |]
        }

newtype RoutesLookupTaskDistance =
    RoutesLookupTaskDistance
        (Maybe (RoutesLookup TaskRouteDistance))

newtype StartGate = StartGate UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype UtcOffset = UtcOffset { timeZoneMinutes :: Int }
    deriving (Eq, Ord, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)

data OpenClose =
    OpenClose
        { open :: UTCTime
        , close :: UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | If all the zone open and close times are the same then we may only be
-- given a singleton list. This function retrieves the open close time
-- for the speed section whether we have a singleton list or a list with
-- elements for each zone.
openClose :: SpeedSection -> [OpenClose] -> Maybe OpenClose
openClose _ [] = Nothing
openClose Nothing (x : _) = Just x
openClose _ [x] = Just x
openClose (Just (_, e)) xs = listToMaybe . take 1 . drop (e - 1) $ xs

pilotNamed :: CompSettings k -> [PilotName] -> [Pilot]
pilotNamed CompSettings{pilots} [] = sort . nub . join $
    (fmap . fmap) (\(PilotTrackLogFile p _) -> p) pilots
pilotNamed CompSettings{pilots} xs = sort . nub . join $
    [ filter (\(Pilot (_, name)) -> name `elem` xs) ps
    | ps <- (fmap . fmap) (\(PilotTrackLogFile p _) -> p) pilots
    ]

data CompSettings k =
    CompSettings
        { comp :: Comp
        , nominal :: Nominal
        , compTweak :: Maybe Tweak
        , tasks :: [Task k]
        , taskFolders :: [TaskFolder]
        , pilots :: [[PilotTrackLogFile]]
        , pilotGroups :: [PilotGroup]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Projection = UTM
    deriving (Eq, Ord, Show)

instance ToJSON Projection where
    toJSON = const $ String "UTM"

instance FromJSON Projection where
    parseJSON _ = return UTM

data EarthMath
    = Pythagorus
    | Haversines
    | Vincenty
    | Andoyer
    deriving (Eq, Ord, Show)

instance ToJSON EarthMath where
    toJSON = toJSON . String . T.pack . show

instance FromJSON EarthMath where
    parseJSON o@(String _) = do
        s :: String <- parseJSON o
        case s of
            "Pythagorus" -> return Pythagorus
            "Haversines" -> return Haversines
            "Vincenty" -> return Vincenty
            "Andoyer" -> return Andoyer
            _ -> empty

    parseJSON _ = empty

data EarthModel
    = EarthAsSphere {radius :: QRadius Double [u| m |]}
    | EarthAsEllipsoid (Ellipsoid Double)
    | EarthAsFlat {projection :: Projection}
    deriving (Eq, Ord, Show, Generic)

earthModelCtorTag :: String -> String
earthModelCtorTag s
    | s == "EarthAsSphere" = "sphere"
    | s == "EarthAsEllipsoid" = "ellipsoid"
    | s == "EarthAsFlat" = "flat"
    | otherwise = s

instance ToJSON EarthModel where
    toJSON = genericToJSON $
        defaultOptions
            { sumEncoding = ObjectWithSingleField
            , constructorTagModifier = earthModelCtorTag
            }

instance FromJSON EarthModel where
    parseJSON = genericParseJSON $
        defaultOptions
            { sumEncoding = ObjectWithSingleField
            , constructorTagModifier = earthModelCtorTag
            }

-- | Groups of pilots for a task.
data PilotGroup =
    PilotGroup
        { absent :: [Pilot]
        -- ^ Pilots absent from a task.
        , dnf :: [Pilot]
        -- ^ Pilots that did not fly.
        , didFlyNoTracklog :: DfNoTrack
        -- ^ Pilots that did fly but have no tracklog. They may have been
        -- awarded a distance by the scorer.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Comp =
    Comp
        { civilId :: String
        , compName :: String
        , discipline :: Discipline
        , location :: String
        , from :: String
        , to :: String
        , utcOffset :: UtcOffset
        , scoreBack :: Maybe (ScoreBackTime (Quantity Double [u| s |]))
        , give :: Maybe Give
        , earth :: EarthModel
        , earthMath :: EarthMath
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Nominal =
    Nominal
        { launch :: NominalLaunch
        , goal :: NominalGoal
        , distance :: NominalDistance (Quantity Double [u| km |])
        , free :: MinimumDistance (Quantity Double [u| km |])
        -- ^ A mimimum distance awarded to pilots that bomb out for 'free'.
        , time :: NominalTime (Quantity Double [u| h |])
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Tweaks that move the scoring away from the default formulae for either
-- discipline.
data Tweak =
    Tweak
        { leadingWeightScaling :: Maybe LwScaling
        , arrivalWeightScaling :: Maybe AwScaling
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

defaultNominal :: Nominal
defaultNominal =
    Nominal
        { launch = NominalLaunch $ 96 % 100
        , goal = NominalGoal $ 25 % 100
        , distance = NominalDistance . MkQuantity $ 70
        , free = MinimumDistance . MkQuantity $ 7
        , time = NominalTime . MkQuantity $ 1.5
        }

data TaskStop =
    TaskStop
        { announced :: UTCTime
        -- ^ The time at which the task was stopped.
        , retroactive :: UTCTime
        -- ^ The time at which the task will be scored until.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Task k =
    Task
        { taskName :: String
        , zones :: Zones
        , speedSection :: SpeedSection
        , zoneTimes :: [OpenClose]
        , startGates :: [StartGate]
        , stopped :: Maybe TaskStop
        , taskTweak :: Maybe Tweak
        , penals :: [(Pilot, [PointPenalty], String)]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

showTask :: Task k -> String
showTask Task {taskName, zones, speedSection, zoneTimes, startGates} =
    unwords [ "Task '" ++ taskName ++ "'"
            , ", zones "
            , intercalate ", " $ showZone <$> raw zones
            , ", speed section "
            , show speedSection
            , ", zone times"
            , show zoneTimes 
            , ", start gates "
            , intercalate ", " $ show <$> startGates 
            ]

instance FieldOrdering (CompSettings k) where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- CompSettings fields
        ("comp", _) -> LT
        ("nominal", "comp") -> GT
        ("nominal", _) -> LT
        ("compTweak", "comp") -> GT
        ("compTweak", "nominal") -> GT
        ("compTweak", _) -> LT
        ("tasks", "taskFolders") -> LT
        ("tasks", "pilots") -> LT
        ("tasks", "pilotGroups") -> LT
        ("tasks", _) -> GT
        ("taskFolders", "pilots") -> LT
        ("taskFolders", "pilotGroups") -> LT
        ("taskFolders", _) -> GT
        ("pilots", "pilotGroups") -> LT
        ("pilots", _) -> GT
        ("pilotGroups", _) -> GT

        -- TaskZones fields
        ("raw", _) -> LT

        ("raceKind", "openKind") -> LT
        ("raceKind", _) -> GT

        ("openKind", _) -> GT

        -- TzEssIsGoal fields
        ("ess-is-goal", _) -> GT

        -- TzEssIsNotGoal fields
        ("prolog", _) -> LT

        ("race", "prolog") -> GT
        ("race", _) -> LT

        ("race-ess", "prolog") -> GT
        ("race-ess", "race") -> GT
        ("race-ess", _) -> LT

        ("ess", "epilogue") -> LT
        ("ess", "goal") -> LT
        ("ess", _) -> GT

        ("epilog", "goal") -> LT
        ("epilog", _) -> GT

        ("goal", "race") -> GT
        ("goal", "ess") -> GT
        ("goal", "epilog") -> GT
        ("goal", "race-ess") -> GT

        -- TzOpenDistance fields
        ("open-mandatory", "open-free") -> LT
        ("open-mandatory", _) -> GT

        ("open-free", _) -> GT

        -- Nominal fields
        ("launch", _) -> LT

        ("goal", "launch") -> GT
        ("goal", "") -> LT

        ("distance", "launch") -> GT
        ("distance", "goal") -> GT
        ("distance", _) -> LT

        ("free", "launch") -> GT
        ("free", "goal") -> GT
        ("free", "distance") -> GT
        ("free", _) -> LT

        ("time", _) -> GT

        -- Comp fields
        ("compName", _) -> LT

        ("discipline", "compName") -> GT
        ("discipline", _) -> LT

        ("location", "compName") -> GT
        ("location", "discipline") -> GT
        ("location", _) -> LT

        ("from", "to") -> LT

        ("civilId", "utcOffset") -> LT
        ("civilId", "scoreBack") -> LT
        ("civilId", "give") -> LT
        ("civilId", "earth") -> LT
        ("civilId", _) -> GT

        ("utcOffset", "scoreBack") -> LT
        ("utcOffset", "give") -> LT
        ("utcOffset", "earth") -> LT
        ("utcOffset", _) -> GT

        ("scoreBack", "give") -> LT
        ("scoreBack", "earth") -> LT
        ("scoreBack", _) -> GT

        ("give", "scoreBack") -> GT

        ("earth", _) -> GT

        -- Task fields
        ("taskName", _) -> LT

        ("zones", "taskName") -> GT
        ("zones", _) -> LT

        ("strict", "raw") -> GT
        ("strict", _) -> LT

        ("kind", "raw") -> GT
        ("kind", "strict") -> GT
        ("kind", _) -> LT

        ("speedSection", "zoneTimes") -> LT
        ("speedSection", "startGates") -> LT
        ("speedSection", "stopped") -> LT
        ("speedSection", "taskTweak") -> LT
        ("speedSection", "penals") -> LT
        ("speedSection", "absent") -> LT
        ("speedSection", _) -> GT

        ("zoneTimes", "startGates") -> LT
        ("zoneTimes", "absent") -> LT
        ("zoneTimes", _) -> GT

        ("startGates", "absent") -> LT
        ("startGates", "taskTweak") -> LT
        ("startGates", "penals") -> LT
        ("startGates", _) -> GT

        ("stopped", "taskTweak") -> LT
        ("stopped", "penals") -> LT
        ("stopped", _) -> GT

        ("taskTweak", "penals") -> LT
        ("taskTweak", _) -> GT

        ("penals", _) -> GT

        ("absent", "didFlyNoTracklog") -> LT
        ("absent", _) -> GT
        ("didFlyNoTracklog", _) -> GT

        -- StartGates fields
        ("open", _) -> LT
        ("close", _) -> GT

        -- Turnpoint and ZoneKind fields
        ("zoneName", _) -> LT

        ("lat", "zoneName") -> GT
        ("lat", _) -> LT

        ("lng", "zoneName") -> GT
        ("lng", "lat") -> GT
        ("lng", _) -> LT

        ("radius", "zoneName") -> GT
        ("radius", "lat") -> GT
        ("radius", "lng") -> GT
        ("radius", "center") -> GT
        ("radius", _) -> LT

        ("give", "zoneName") -> GT
        ("give", "lat") -> GT
        ("give", "lng") -> GT
        ("give", "radius") -> GT
        ("give", _) -> LT

        ("alt", _) -> GT

        -- Give fields
        ("giveFraction", _) -> LT
        ("giveDistance", _) -> GT

        -- DfNoTrackPilot fields
        ("pilot", "awardedReach") -> LT
        ("pilot", "awardedVelocity") -> LT

        ("awardedReach", "awardedVelocity") -> LT
        ("awardedReach", _) -> GT

        ("awardedVelocity", _) -> GT

        -- AwardedDistance fields
        ("awardedMade", "awardedTask") -> LT
        ("awardedMade", "awardedFrac") -> LT

        ("awardedTask", "awardedMade") -> GT
        ("awardedTask", "awardedFrac") -> LT

        ("awardedFrac", _) -> GT

        -- AwardedVelocity fields
        ("ss", "es") -> LT
        ("es", "ss") -> GT

        _ -> compare a b
