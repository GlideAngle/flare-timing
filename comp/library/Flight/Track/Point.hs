{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Point
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Task points.
-}
module Flight.Track.Point
    ( Velocity(..)
    , NormBreakdown(..)
    , Breakdown(..)
    , NormPointing(..)
    , Pointing(..)
    , Allocation(..)
    ) where

import Data.Time.Clock (UTCTime)
import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (QAlt)
import Flight.Field (FieldOrdering(..))
import Flight.Score
    ( GoalRatio, TaskPlacing
    , TaskPoints, Points
    , DistancePoints, LeadingPoints, ArrivalPoints, TimePoints
    , Validity, ValidityWorking, Weights
    , LaunchValidityWorking
    , Pilot, BestTime, PilotTime, PilotDistance, PilotVelocity
    , PointPenalty
    , SpeedFraction
    , ArrivalFraction
    , LeadingArea(..), LeadingCoef(..), LeadingFraction(..)
    )
import Flight.Track.Distance (Land)
import Flight.Comp (StartGate)

data Velocity =
    Velocity
        { ss :: Maybe UTCTime
          -- ^ The time the pilot crossed the start and started the speed
          -- section.
        , gs :: Maybe StartGate
          -- ^ The time the pilot was deemed to have started when there are
          -- start gates. This is the opening time of the start gate that the
          -- pilot took.
        , es :: Maybe UTCTime
        , ssElapsed :: Maybe (PilotTime (Quantity Double [u| h |]))
          -- ^ The elapsed time from the moment the pilot crossed the start.
        , gsElapsed :: Maybe (PilotTime (Quantity Double [u| h |]))
          -- ^ The elapsed time from the start gate. Always as long as
          -- @ssElapsed@.
        , ssDistance :: Maybe (PilotDistance (Quantity Double [u| km |]))
          -- ^ The best distance the pilot made over the speed section, not
          -- exceeding goal and may be further than where the pilot landed. The
          -- velocities over the speed section use this distance.
        , ssVelocity :: Maybe (PilotVelocity (Quantity Double [u| km / h |]))
          -- ^ The velocity from the time the started the speed section.
        , gsVelocity :: Maybe (PilotVelocity (Quantity Double [u| km / h |]))
          -- ^ The velocity from the start gate time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The breakdown of the expected or normative score for a pilot for a task
-- extracted from the *.fsdb file.
data NormBreakdown =
    NormBreakdown
        { place :: TaskPlacing
        , total :: TaskPoints
        , distance :: DistancePoints
        , leading :: LeadingPoints
        , arrival :: ArrivalPoints
        , time :: TimePoints
        , distanceMade :: Land
        , distanceFrac :: Double
        , ss :: Maybe UTCTime
        , es :: Maybe UTCTime
        , timeElapsed :: Maybe (PilotTime (Quantity Double [u| h |]))
        , timeFrac :: SpeedFraction
        , leadingArea :: LeadingArea (Quantity Double [u| (km^2)*s |])
        , leadingCoef :: LeadingCoef (Quantity Double [u| 1 |])
        , leadingFrac :: LeadingFraction
        , arrivalFrac :: ArrivalFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Breakdown =
    Breakdown
        { place :: TaskPlacing
        , total :: TaskPoints
        -- ^ The total points, the sum of the parts in the breakdown with any
        -- penalties applied, with fractional ones applied before absolute ones.
        , penalties :: [PointPenalty]
        , penaltyReason :: String
        , breakdown :: Points
        , velocity :: Maybe Velocity
        , reachDistance :: Maybe (PilotDistance (Quantity Double [u| km |]))
          -- ^ The best distance the pilot made, not exceeding goal and may be
          -- further than where the pilot landed. The linear distance points
          -- are awarded from this distance.
        , landedDistance :: Maybe (PilotDistance (Quantity Double [u| km |]))
          -- ^ The distance along the course to where the pilot landed.
        , stoppedAlt :: Maybe (QAlt Double [u| m |])
          -- ^ The altitude of the pilot at the stopped task score back time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the expected or normative points for that task as scored by
-- FS.
data NormPointing =
    NormPointing
        { bestTime :: [Maybe (BestTime (Quantity Double [u| h |]))]
        , validityWorkingLaunch :: [Maybe LaunchValidityWorking]
        , validity :: [Maybe Validity]
        , score :: [[(Pilot, NormBreakdown)]]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the points for that task.
data Pointing =
    Pointing
        { validityWorking :: [Maybe ValidityWorking]
        , validity :: [Maybe Validity]
        , allocation :: [Maybe Allocation]
        , score :: [[(Pilot, Breakdown)]]
        , scoreDf :: [[(Pilot, Breakdown)]]
        , scoreDfNoTrack :: [[(Pilot, Breakdown)]]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Allocation =
    Allocation 
        { goalRatio :: GoalRatio
        , weight :: Weights
        , points :: Points
        , taskPoints :: TaskPoints
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering Pointing where
    fieldOrder _ = cmpPointing

instance FieldOrdering NormPointing where
    fieldOrder _ = cmpNorm

cmpNorm :: (Ord a, IsString a) => a -> a -> Ordering
cmpNorm a b =
    case (a, b) of
        ("place", _) -> LT

        ("total", "place") -> GT
        ("total", _) -> LT

        ("distance", "place") -> GT
        ("distance", "total") -> GT
        ("distance", _) -> LT

        ("leading", "place") -> GT
        ("leading", "total") -> GT
        ("leading", "distance") -> GT
        ("leading", _) -> LT

        ("arrival", "place") -> GT
        ("arrival", "total") -> GT
        ("arrival", "distance") -> GT
        ("arrival", "leading") -> GT
        ("arrival", _) -> LT

        ("time", "place") -> GT
        ("time", "total") -> GT
        ("time", "distance") -> GT
        ("time", "leading") -> GT
        ("time", "arrival") -> GT
        ("time", _) -> LT

        ("distanceMade", "distanceFrac") -> LT
        ("distanceMade", "ss") -> LT
        ("distanceMade", "es") -> LT
        ("distanceMade", "timeElapsed") -> LT
        ("distanceMade", "timeFrac") -> LT
        ("distanceMade", "leadingArea") -> LT
        ("distanceMade", "leadingCoef") -> LT
        ("distanceMade", "leadingFrac") -> LT
        ("distanceMade", "arrivalFrac") -> LT
        ("distanceMade", _) -> GT

        ("distanceFrac", "ss") -> LT
        ("distanceFrac", "es") -> LT
        ("distanceFrac", "timeElapsed") -> LT
        ("distanceFrac", "timeFrac") -> LT
        ("distanceFrac", "leadingArea") -> LT
        ("distanceFrac", "leadingCoef") -> LT
        ("distanceFrac", "leadingFrac") -> LT
        ("distanceFrac", "arrivalFrac") -> LT
        ("distanceFrac", _) -> GT

        ("ss", "es") -> LT
        ("ss", "timeElapsed") -> LT
        ("ss", "timeFrac") -> LT
        ("ss", "leadingArea") -> LT
        ("ss", "leadingCoef") -> LT
        ("ss", "leadingFrac") -> LT
        ("ss", "arrivalFrac") -> LT
        ("ss", _) -> GT

        ("es", "timeElapsed") -> LT
        ("es", "timeFrac") -> LT
        ("es", "leadingArea") -> LT
        ("es", "leadingCoef") -> LT
        ("es", "leadingFrac") -> LT
        ("es", "arrivalFrac") -> LT
        ("es", _) -> GT

        ("timeElapsed", "timeFrac") -> LT
        ("timeElapsed", "leadingArea") -> LT
        ("timeElapsed", "leadingCoef") -> LT
        ("timeElapsed", "leadingFrac") -> LT
        ("timeElapsed", "arrivalFrac") -> LT
        ("timeElapsed", _) -> GT

        ("timeFrac", "leadingArea") -> LT
        ("timeFrac", "leadingCoef") -> LT
        ("timeFrac", "leadingFrac") -> LT
        ("timeFrac", "arrivalFrac") -> LT
        ("timeFrac", _) -> GT

        ("leadingArea", "leadingCoef") -> LT
        ("leadingArea", "leadingFrac") -> LT
        ("leadingArea", "arrivalFrac") -> LT
        ("leadingArea", _) -> GT

        ("leadingCoef", "leadingFrac") -> LT
        ("leadingCoef", "arrivalFrac") -> LT
        ("leadingCoef", _) -> GT

        ("leadingFrac", "arrivalFrac") -> LT
        ("leadingFrac", _) -> GT

        ("arrivalFrac", _) -> GT

        ("bestTime", _) -> LT
        ("score", _) -> GT

        _ -> compare a b

cmpPointing :: (Ord a, IsString a) => a -> a -> Ordering
cmpPointing a b =
    case (a, b) of
        -- Breakdown fields
        ("place", _) -> LT

        ("total", "place") -> GT
        ("total", _) -> LT

        ("penalties", "place") -> GT
        ("penalties", "total") -> GT
        ("penalties", _) -> LT

        ("penaltyReason", "place") -> GT
        ("penaltyReason", "total") -> GT
        ("penaltyReason", "penalties") -> GT
        ("penaltyReason", _) -> LT

        ("breakdown", "place") -> GT
        ("breakdown", "total") -> GT
        ("breakdown", "penalties") -> GT
        ("breakdown", "penaltyReason") -> GT
        ("breakdown", _) -> LT

        ("velocity", "place") -> GT
        ("velocity", "total") -> GT
        ("velocity", "penalties") -> GT
        ("velocity", "penaltyReason") -> GT
        ("velocity", "breakdown") -> GT
        ("velocity", _) -> LT

        ("reachDistance", "place") -> GT
        ("reachDistance", "total") -> GT
        ("reachDistance", "penalties") -> GT
        ("reachDistance", "penaltyReason") -> GT
        ("reachDistance", "breakdown") -> GT
        ("reachDistance", "velocity") -> GT
        ("reachDistance", _) -> LT

        ("landedDistance", "stoppedAlt") -> LT
        ("landedDistance", _) -> GT

        ("stoppedAlt", _) -> GT

        -- Velocity fields
        ("ss", _) -> LT

        ("gs", "ss") -> GT
        ("gs", _) -> LT

        ("es", "ss") -> GT
        ("es", "gs") -> GT
        ("es", _) -> LT

        ("ssDistance", "ss") -> GT
        ("ssDistance", "gs") -> GT
        ("ssDistance", "es") -> GT
        ("ssDistance", _) -> LT

        -- NOTE: Duplicate record fields.
        -- ("distance", _) -> LT

        ("ssElapsed", "ss") -> GT
        ("ssElapsed", "gs") -> GT
        ("ssElapsed", "es") -> GT
        ("ssElapsed", "ssDistance") -> GT
        ("ssElapsed", _) -> LT

        ("gsElapsed", "ss") -> GT
        ("gsElapsed", "gs") -> GT
        ("gsElapsed", "es") -> GT
        ("gsElapsed", "ssDistance") -> GT
        ("gsElapsed", "ssElapsed") -> GT
        ("gsElapsed", _) -> LT

        ("ssVelocity", "gsVelocity") -> LT
        ("ssVelocity", _) -> GT

        ("gsVelocity", _) -> GT

        -- Pointing fields
        ("validityWorking", _) -> LT

        ("validity", "validityWorking") -> GT
        ("validity", _) -> LT

        ("allocation", _) -> GT

        -- Allocation fields
        ("goalRatio", _) -> LT

        ("weight", "goalRatio") -> GT
        ("weight", _) -> LT

        ("points", "goalRatio") -> GT
        ("points", "weight") -> GT
        ("points", _) -> LT

        ("taskPoints", "goalRatio") -> GT
        ("taskPoints", "weight") -> GT
        ("taskPoints", "points") -> GT
        ("taskPoints", _) -> LT

        ("score", "scoreDf") -> LT
        ("score", "scoreDfNoTrack") -> LT
        ("score", _) -> GT

        ("scoreDf", "DfNoTrack") -> LT
        ("scoreDf", _) -> GT

        ("scoreDfNoTrack", _) -> GT

        -- DistanceValidityWorking fields
        ("sum", _) -> LT

        ("flying", "sum") -> GT
        ("flying", "nominalLaunch") -> GT
        ("flying", _) -> LT

        ("area", "sum") -> GT
        ("area", "flying") -> GT
        ("area", _) -> LT

        ("nominalGoal", "sum") -> GT
        ("nominalGoal", "flying") -> GT
        ("nominalGoal", "area") -> GT
        ("nominalGoal", _) -> LT

        ("nominalDistance", "sum") -> GT
        ("nominalDistance", "flying") -> GT
        ("nominalDistance", "area") -> GT
        ("nominalDistance", "nominalGoal") -> GT

        -- NOTE: Duplicate record fields, see TimeValidityWorking for wildcard.
        -- ("nominalDistance", _) -> LT

        ("minimumDistance", "sum") -> GT
        ("minimumDistance", "flying") -> GT
        ("minimumDistance", "area") -> GT
        ("minimumDistance", "nominalGoal") -> GT
        ("minimumDistance", "nominalDistance") -> GT
        ("minimumDistance", _) -> LT

        ("best", _) -> GT

        -- LaunchValidityWorking fields
        ("nominalLaunch", _) -> LT

        ("present", "flying") -> GT
        ("present", "nominalLaunch") -> GT
        ("present", _) -> LT

        -- TimeValidityWorking fields
        ("nominalTime", _) -> LT

        ("ssBestTime", "nominalTime") -> GT
        ("ssBestTime", _) -> LT

        ("gsBestTime", "nominalTime") -> GT
        ("gsBestTime", "ssBestTime") -> GT
        ("gsBestTime", _) -> LT

        ("nominalDistance", "nominalTime") -> GT
        ("nominalDistance", "ssBestTime") -> GT
        ("nominalDistance", "gsBestTime") -> GT
        ("nominalDistance", _) -> LT

        ("bestDistance", _) -> GT

        -- Point fields
        ("reach", _) -> LT

        ("effort", "reach") -> GT
        ("effort", _) -> LT

        ("distance", "reach") -> GT
        ("distance", "effort") -> GT
        ("distance", _) -> LT

        ("leading", "reach") -> GT
        ("leading", "effort") -> GT
        ("leading", "distance") -> GT
        ("leading", _) -> LT

        ("arrival", "reach") -> GT
        ("arrival", "effort") -> GT
        ("arrival", "distance") -> GT
        ("arrival", "leading") -> GT
        ("arrival", _) -> LT

        ("time", "reach") -> GT
        ("time", "effort") -> GT
        ("time", "distance") -> GT
        ("time", "leading") -> GT
        ("time", "arrival") -> GT
        ("time", _) -> LT

        -- Validity fields
        ("task", _) -> LT

        ("launch", "task") -> GT
        ("launch", _) -> LT

        -- NOTE: Duplicate field names between Point and Validity, redundant pattern.
        -- ("distance", "task") -> GT
        -- ("distance", "launch") -> GT
        -- ("distance", _) -> LT
        -- ("time", _) -> GT

        -- NOTE: Weight fields catered for by Point fields

        _ -> compare a b
