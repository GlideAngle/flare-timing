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
    , RoutesLookup
    , TaskRouteDistance(..)
    , TimePass
    , routeLengthOfSs
    , showTask
    , openClose
    , unpackOpenClose
    , speedSectionToLeg
    -- * EarlyStart
    , OpenClose(..)
    , EarlyStart(..)
    , nullEarlyStart
    , gateTimeCheck
    , zoneTimeCheck
    , timeCheck
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
    , dfNoTrackReach
    , pilotNamed
    -- * Comp paths
    , module Flight.Path
    ) where

import Data.Refined (assumeProp, refined)
import Data.Ratio ((%))
import Control.Monad (join)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock (addUTCTime)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Maybe (listToMaybe)
import Data.List (intercalate, nub, sort)
import Data.String (IsString())
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Zone.MkZones (Zones(..), Discipline(..))
import Flight.Zone.Raw (Give, showZone)
import Flight.Field (FieldOrdering(..))
import Flight.Pilot
import Flight.Path
import Flight.Distance (TaskDistance(..), QTaskDistance)
import "flight-gap-allot" Flight.Score
    ( NominalLaunch(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalTime(..)
    , PilotId(..)
    , PilotName(..)
    , Pilot(..)
    )
import "flight-gap-lead" Flight.Score (Leg(..), LengthOfSs(..))
import "flight-gap-stop" Flight.Score (ScoreBackTime(..))
import "flight-gap-math" Flight.Score
    ( PenaltySeqs(..)
    , SecondsPerPoint(..)
    , JumpTheGunLimit(..)
    , TooEarlyPoints(..)
    )
import "flight-gap-weight" Flight.Score (LwScaling(..), EGwScaling(..))
import Flight.Geodesy (EarthMath(..), EarthModel(..))

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
        , launchToSssDistance :: QTaskDistance Double [u| m |]
        }

routeLengthOfSs :: TaskRouteDistance -> LengthOfSs
routeLengthOfSs TaskRouteDistance{speedSubsetDistance = TaskDistance d} =
    let dKm :: Quantity Double [u| km |] = convert d
        dKm' :: Quantity Rational [u| km |] = toRational' dKm
    in LengthOfSs dKm'

newtype RoutesLookupTaskDistance =
    RoutesLookupTaskDistance
        (Maybe (RoutesLookup TaskRouteDistance))

newtype StartGate = StartGate UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

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

data EarlyStart =
    EarlyStart
        { earliest :: JumpTheGunLimit (Quantity Double [u| s |])
        , earlyPenalty :: SecondsPerPoint (Quantity Double [u| s |])
        , tooEarlyReset :: TooEarlyPoints
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

nullEarlyStart :: EarlyStart
nullEarlyStart =
    EarlyStart
        (JumpTheGunLimit [u| 0 s |])
        (SecondsPerPoint [u| 0 s |])
        (TooEarlyPoints (assumeProp $ refined 0))

-- | If all the zone open and close times are the same then we may only be
-- given a singleton list. This function retrieves the open close time
-- for the speed section whether we have a singleton list or a list with
-- elements for each zone.
openClose :: SpeedSection -> [OpenClose] -> Maybe OpenClose
openClose _ [] = Nothing
openClose Nothing (x : _) = Just x
openClose _ [x] = Just x
openClose (Just (_, e)) xs = listToMaybe . take 1 . drop (e - 1) $ xs

-- | If all the zone open and close times are the same then we may only be
-- given a singleton list. This function expands that list.
unpackOpenClose :: [OpenClose] -> [Maybe OpenClose]
unpackOpenClose = \case
    [] -> repeat Nothing
    [oc] -> Just <$> repeat oc
    ocs -> Just <$> ocs

type TimePass = UTCTime -> Bool

gateTimeCheck :: JumpTheGunLimit (Quantity Double [u| s |]) -> StartGate -> TimePass
gateTimeCheck (JumpTheGunLimit (MkQuantity limit)) (StartGate sg) = \t ->
    let limit' = negate $ fromIntegral (round limit :: Int)
        earliestStart = limit' `addUTCTime` sg
        notEarly = earliestStart <= t

    in notEarly

-- | If a scorer sets the gate open time the same as the first start gate time
-- then for jump the gun to work pilots must be able to start before the gate
-- open time.
zoneTimeCheck :: JumpTheGunLimit (Quantity Double [u| s |]) -> OpenClose -> TimePass
zoneTimeCheck (JumpTheGunLimit (MkQuantity limit)) OpenClose{open, close} = \t ->
    let notLate = t <= close
        limit' = negate $ fromIntegral (round limit :: Int)
        earliestStart = limit' `addUTCTime` open
        notEarly = earliestStart <= t

    in notEarly && notLate

timeCheck :: EarlyStart -> [StartGate] -> [OpenClose] -> [TimePass]
timeCheck EarlyStart{earliest} startGates zoneTimes =
    [
        let zoneCheck = maybe (const True) (zoneTimeCheck earliest) oc
            gateChecks = gateTimeCheck earliest <$> startGates
            gatesCheck t = null startGates || (or $ ($ t) <$> gateChecks)
        in (\t -> zoneCheck t && gatesCheck t)

    | oc <- unpackOpenClose zoneTimes
    ]

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
        , earth :: EarthModel Double
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
        , leadingAreaDistanceSquared :: Bool
        , arrivalRank :: Bool
        , arrivalTime :: Bool
        , essNotGoalScaling :: EGwScaling
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
        , cancelled :: Bool
        -- ^ If the task is cancelled. A task can be stopped and then later
        -- cancelled.
        , taskTweak :: Maybe Tweak
        , earlyStart :: EarlyStart
        , penalsAuto :: [(Pilot, PenaltySeqs, String)]
        , penals :: [(Pilot, PenaltySeqs, String)]
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
        ("speedSection", "cancelled") -> LT
        ("speedSection", "taskTweak") -> LT
        ("speedSection", "earlyStart") -> LT
        ("speedSection", "penalsAuto") -> LT
        ("speedSection", "penals") -> LT
        ("speedSection", "absent") -> LT
        ("speedSection", _) -> GT

        ("zoneTimes", "startGates") -> LT
        ("zoneTimes", "stopped") -> LT
        ("zoneTimes", "cancelled") -> LT
        ("zoneTimes", "taskTweak") -> LT
        ("zoneTimes", "earlyStart") -> LT
        ("zoneTimes", "penalsAuto") -> LT
        ("zoneTimes", "penals") -> LT
        ("zoneTimes", "absent") -> LT
        ("zoneTimes", _) -> GT

        ("startGates", "stopped") -> LT
        ("startGates", "cancelled") -> LT
        ("startGates", "absent") -> LT
        ("startGates", "taskTweak") -> LT
        ("startGates", "earlyStart") -> LT
        ("startGates", "penalsAuto") -> LT
        ("startGates", "penals") -> LT
        ("startGates", _) -> GT

        ("stopped", "cancelled") -> LT
        ("stopped", "taskTweak") -> LT
        ("stopped", "earlyStart") -> LT
        ("stopped", "penalsAuto") -> LT
        ("stopped", "penals") -> LT
        ("stopped", _) -> GT

        ("cancelled", "taskTweak") -> LT
        ("cancelled", "earlyStart") -> LT
        ("cancelled", "penalsAuto") -> LT
        ("cancelled", "penals") -> LT
        ("cancelled", _) -> GT

        ("taskTweak", "earlyStart") -> LT
        ("taskTweak", "penalsAuto") -> LT
        ("taskTweak", "penals") -> LT
        ("taskTweak", _) -> GT

        ("earlyStart", "penalsAuto") -> LT
        ("earlyStart", "penals") -> LT
        ("earlyStart", _) -> GT

        ("penalsAuto", "penals") -> LT
        ("penalsAuto", _) -> GT

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

        ("giveIn", "zoneName") -> GT
        ("giveIn", "lat") -> GT
        ("giveIn", "lng") -> GT
        ("giveIn", "radius") -> GT
        ("giveIn", _) -> LT

        ("giveOut", "zoneName") -> GT
        ("giveOut", "lat") -> GT
        ("giveOut", "lng") -> GT
        ("giveOut", "radius") -> GT
        ("giveOut", _) -> LT

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
