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
    , EarthModel(..)
    , Comp(..)
    , Nominal(..)
    , UtcOffset(..)
    , defaultNominal
    -- * Task
    , Task(..)
    , TaskStop(..)
    , IxTask(..)
    , Zones(..)
    , SpeedSection
    , StartGate(..)
    , OpenClose(..)
    , FirstLead(..)
    , FirstStart(..)
    , LastArrival(..)
    , StartEnd(..)
    , StartEndMark
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
    , FlyingSection
    , pilotNamed
    -- * Comp paths
    , module Flight.Path
    ) where

import Data.Ratio ((%))
import Control.Monad (join)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Maybe (listToMaybe)
import Data.List (intercalate, nub, sort)
import Data.String (IsString())
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (QRadius)
import Flight.Zone.MkZones (Zones(..), Discipline(..), SpeedSection)
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

-- | The time of last crossing of the end of the speed section. This won't
-- exist if no one makes goal and everyone lands out.
newtype LastArrival = LastArrival UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A race task can be started and not finished if no one makes goal.
data StartEnd a b =
    StartEnd
        { unStart :: a
        , unEnd :: Maybe b
        }
    deriving Show

type StartEndMark = StartEnd UTCTime UTCTime

-- | 1-based indices of a task in a competition.
newtype IxTask = IxTask Int deriving (Eq, Show)

speedSectionToLeg :: SpeedSection -> Int -> Leg
speedSectionToLeg Nothing i = RaceLeg i
speedSectionToLeg (Just (s, e)) i =
    if | i < s -> PrologLeg i
       | i > e -> EpilogLeg i
       | True -> RaceLeg i

-- | A pair into the list of fixes marking those deemed logged while flying.
-- These could be indices, seconds offsets or UTC times.
type FlyingSection a = Maybe (a, a)

type RoutesLookup a = IxTask -> Maybe a

data TaskRouteDistance =
    TaskRouteDistance
        { wholeTaskDistance :: QTaskDistance Double [u| m |]
        , speedSubsetDistance :: QTaskDistance Double [u| m |]
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
        , tasks :: [Task k]
        , taskFolders :: [TaskFolder]
        , pilots :: [[PilotTrackLogFile]]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EarthModel
    = EarthAsSphere (QRadius Double [u| m |])
    | EarthAsEllipsoid (Ellipsoid Double)
    | EarthAsFlat 
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
        , earthModel :: EarthModel
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

defaultNominal :: Nominal
defaultNominal =
    Nominal
        { launch = NominalLaunch $ 96 % 100
        , goal = NominalGoal $ 25 % 100
        , distance = NominalDistance . MkQuantity $ 70
        , free = MinimumDistance . MkQuantity $ 7
        , time = NominalTime . MkQuantity $ 1.5
        }

newtype TaskStop =
    TaskStop
        { announced :: UTCTime
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
        , absent :: [Pilot]
        -- ^ Pilots absent from this task.
        , stopped :: Maybe TaskStop
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
        ("tasks", "taskFolders") -> LT
        ("tasks", "pilots") -> LT
        ("tasks", _) -> GT
        ("taskFolders", "pilots") -> LT
        ("taskFolders", _) -> GT
        ("pilots", _) -> GT

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

        ("civilId", "scoreBack") -> LT
        ("civilId", "give") -> LT
        ("civilId", "utcOffset") -> LT
        ("civilId", _) -> GT

        ("utcOffset", "scoreBack") -> LT
        ("utcOffset", "give") -> LT
        ("utcOffset", _) -> GT

        ("scoreBack", "give") -> LT
        ("scoreBack", _) -> GT

        ("give", "scoreBack") -> GT

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
        ("speedSection", "absent") -> LT
        ("speedSection", _) -> GT

        ("zoneTimes", "startGates") -> LT
        ("zoneTimes", "absent") -> LT
        ("zoneTimes", _) -> GT

        ("startGates", "absent") -> LT
        ("startGates", _) -> GT
        ("absent", _) -> GT

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

        _ -> compare a b
