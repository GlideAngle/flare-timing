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
    , Breakdown(..)
    , AltBreakdown(..)
    , AirScoreBreakdown(..)
    , airScoreToAltBreakdown
    , AlternativePointing(..)
    , AltPointing
    , AirScorePointing
    , TaskPointing(..), CompPointing(..)
    , Allocation(..)
    , EssNotGoal(..)
    , mkCompGapPoint, unMkCompGapPoint
    ) where

import Data.List (unzip6)
import Data.Time.Clock (UTCTime)
import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (QAlt)
import Flight.Field (FieldOrdering(..))
import "flight-gap-allot" Flight.Score
    ( TaskPlacing, Fractions(..)
    , Pilot, BestTime, PilotTime, PilotDistance, PilotVelocity
    )
import "flight-gap-lead" Flight.Score (LeadingArea(..), LeadingCoef(..))
import "flight-gap-math" Flight.Score
    ( TaskPoints, Points
    , PenaltySeqs
    , JumpedTheGun(..)
    )
import "flight-gap-valid" Flight.Score
    ( Validity, ValidityWorking
    , LaunchValidityWorking
    , TimeValidityWorking
    , DistanceValidityWorking
    , StopValidityWorking
    , ReachToggle(..)
    )
import "flight-gap-weight" Flight.Score (GoalRatio, Weights)
import Flight.Track.Distance (Effort)
import Flight.Comp (StartGate)
import Flight.Track.Curry (uncurry6)

newtype EssNotGoal = EssNotGoal Bool
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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

-- | The breakdown for a pilot for a task extracted from
-- http://localhost:5000/flaretiming_yaml/<comp-id>/gap-score.
data AirScoreBreakdown =
    AirScoreBreakdown
        { place :: TaskPlacing
        -- ^ The place given by the alternative scoring program.
        , total :: TaskPoints
        , breakdown :: Points
        , fractions :: Fractions
        , reach :: ReachToggle (Maybe Effort)
        -- ^ Most pilots have reach but some get nulls from airScore.
        , landedMade :: Maybe Effort
        -- ^ Reported by FS but not by airScore. Not actually used in
        -- calculating points in GAP.
        , ss :: Maybe UTCTime
        , es :: Maybe UTCTime
        , timeElapsed :: Maybe (PilotTime (Quantity Double [u| h |]))
        , leadingArea :: Maybe (LeadingArea (Quantity Double [u| (km^2)*s |]))
        -- ^ Reported by FS but not by airScore.
        , leadingCoef :: LeadingCoef (Quantity Double [u| 1 |])
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The breakdown of the expected or normative score for a pilot for a task
-- extracted from the *.fsdb file.
data AltBreakdown =
    AltBreakdown
        { placeGiven :: TaskPlacing
        -- ^ The place given by the alternative scoring program.
        , placeTaken :: TaskPlacing
        -- ^ The place taken by sorting on total.
        , total :: TaskPoints
        , breakdown :: Points
        , fractions :: Fractions
        , reach :: ReachToggle (Maybe Effort)
        -- ^ Most pilots have reach but some get nulls from airScore.
        , landedMade :: Maybe Effort
        -- ^ Reported by FS but not by airScore. Not actually used in
        -- calculating points in GAP.
        , ss :: Maybe UTCTime
        , es :: Maybe UTCTime
        , timeElapsed :: Maybe (PilotTime (Quantity Double [u| h |]))
        , leadingArea :: Maybe (LeadingArea (Quantity Double [u| (km^2)*s |]))
        -- ^ Reported by FS but not by airScore.
        , leadingCoef :: LeadingCoef (Quantity Double [u| 1 |])
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

airScoreToAltBreakdown :: AirScoreBreakdown -> AltBreakdown
airScoreToAltBreakdown AirScoreBreakdown{..} =
    AltBreakdown
        { placeGiven = place
        , placeTaken = place
        , ..
        }

data Breakdown =
    Breakdown
        { place :: TaskPlacing
        , subtotal :: TaskPoints
        -- ^ The total points without any penalties applied.
        , demeritFrac :: TaskPoints
        -- ^ The effective points removed from applying fractional penalties.
        , demeritPoint :: TaskPoints
        -- ^ The effective points removed from applying point penalties. Points
        -- removed will not take the total points to less than zero.
        , demeritReset :: TaskPoints
        -- ^ The effective points removed from applying reset penalties. Points
        -- removed will not take the total points to less than zero.
        , total :: TaskPoints
        -- ^ The total points, the sum of the parts in the breakdown with any
        -- penalties applied, with fractional ones applied before absolute ones.
        , essNotGoal :: Maybe EssNotGoal
        -- ^ If the pilot has a tracklog, true if they made ESS but not goal.
        , penaltiesEssNotGoal :: PenaltySeqs
        , jump :: Maybe (JumpedTheGun (Quantity Double [u| s |]))
        , penaltiesJumpRaw :: Maybe PenaltySeqs
        , penaltiesJumpEffective :: PenaltySeqs
        , penalties :: PenaltySeqs
        , penaltyReason :: String
        , breakdown :: Points
        , velocity :: Maybe Velocity
        , reach :: Maybe (ReachToggle (PilotDistance (Quantity Double [u| km |])))
          -- ^ The best distance the pilot made, not exceeding goal and may be
          -- further than where the pilot landed. The linear distance points
          -- are awarded from this distance.
        , landedMade :: Maybe (PilotDistance (Quantity Double [u| km |]))
          -- ^ The distance along the course to where the pilot landed.
        , stoppedAlt :: Maybe (QAlt Double [u| m |])
          -- ^ The altitude of the pilot at the stopped task score back time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the expected or normative points for that task as scored by
-- FS.
data AlternativePointing breakdown =
    AlternativePointing
        { bestTime :: [Maybe (BestTime (Quantity Double [u| h |]))]
        , validityWorkingLaunch :: [Maybe LaunchValidityWorking]
        , validityWorkingTime :: [Maybe TimeValidityWorking]
        , validityWorkingDistance :: [Maybe DistanceValidityWorking]
        , validityWorkingStop :: [Maybe StopValidityWorking]
        , validity :: [Maybe Validity]
        , score :: [[(Pilot, breakdown)]]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type AltPointing = AlternativePointing AltBreakdown
type AirScorePointing = AlternativePointing AirScoreBreakdown

data TaskPointing =
    TaskPointing
        { validityWorking :: Maybe ValidityWorking
        , validity :: Maybe Validity
        , allocation :: Maybe Allocation
        , score :: [(Pilot, Breakdown)]
        , scoreDf :: [(Pilot, Breakdown)]
        , scoreDfNoTrack :: [(Pilot, Breakdown)]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the points for that task.
data CompPointing =
    CompPointing
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

mkCompGapPoint :: [TaskPointing] -> CompPointing
mkCompGapPoint ts =
    uncurry6 CompPointing $ unzip6
    [ (a, b, c, d, e, f)
    | TaskPointing
        { validityWorking = a
        , validity = b
        , allocation = c
        , score = d
        , scoreDf = e
        , scoreDfNoTrack = f
        } <- ts
    ]

unMkCompGapPoint :: CompPointing -> [TaskPointing]
unMkCompGapPoint
    CompPointing
        { validityWorking = as
        , validity = bs
        , allocation = cs
        , score = ds
        , scoreDf = es
        , scoreDfNoTrack = fs
        } =
    [ TaskPointing a b c d e f
    | a <- as
    | b <- bs
    | c <- cs
    | d <- ds
    | e <- es
    | f <- fs
    ]

instance FieldOrdering TaskPointing where fieldOrder _ = cmpPointing
instance FieldOrdering CompPointing where fieldOrder _ = cmpPointing
instance FieldOrdering AltPointing where fieldOrder _ = cmpAlt

cmpAlt :: (Ord a, IsString a) => a -> a -> Ordering
cmpAlt a b =
    case (a, b) of
        ("place", _) -> LT

        ("placeGiven", _) -> LT

        ("placeTaken", "PlaceGiven") -> LT
        ("placeTaken", _) -> LT

        ("total", "place") -> GT
        ("total", "placeGiven") -> GT
        ("total", "placeTaken") -> GT
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

        ("reach", "landedMade") -> LT
        ("reach", "distanceFrac") -> LT
        ("reach", "ss") -> LT
        ("reach", "es") -> LT
        ("reach", "timeElapsed") -> LT
        ("reach", "timeFrac") -> LT
        ("reach", "leadingArea") -> LT
        ("reach", "leadingCoef") -> LT
        ("reach", "leadingFrac") -> LT
        ("reach", "arrivalFrac") -> LT
        ("reach", _) -> GT

        ("landedMade", "distanceFrac") -> LT
        ("landedMade", "ss") -> LT
        ("landedMade", "es") -> LT
        ("landedMade", "timeElapsed") -> LT
        ("landedMade", "timeFrac") -> LT
        ("landedMade", "leadingArea") -> LT
        ("landedMade", "leadingCoef") -> LT
        ("landedMade", "leadingFrac") -> LT
        ("landedMade", "arrivalFrac") -> LT
        ("landedMade", _) -> GT

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

        ("essNotGoal", "place") -> GT
        ("essNotGoal", "total") -> GT
        ("essNotGoal", _) -> LT

        ("penaltiesEssNotGoal", "place") -> GT
        ("penaltiesEssNotGoal", "total") -> GT
        ("penaltiesEssNotGoal", "essNotGoal") -> GT
        ("penaltiesEssNotGoal", _) -> LT

        ("jump", "place") -> GT
        ("jump", "total") -> GT
        ("jump", "essNotGoal") -> GT
        ("jump", "penaltiesEssNotGoal") -> GT
        ("jump", _) -> LT

        ("penaltiesJumpRaw", "place") -> GT
        ("penaltiesJumpRaw", "total") -> GT
        ("penaltiesJumpRaw", "essNotGoal") -> GT
        ("penaltiesJumpRaw", "penaltiesEssNotGoal") -> GT
        ("penaltiesJumpRaw", "jump") -> GT
        ("penaltiesJumpRaw", _) -> LT

        ("penaltiesJumpEffective", "place") -> GT
        ("penaltiesJumpEffective", "total") -> GT
        ("penaltiesJumpEffective", "essNotGoal") -> GT
        ("penaltiesJumpEffective", "penaltiesEssNotGoal") -> GT
        ("penaltiesJumpEffective", "jump") -> GT
        ("penaltiesJumpEffective", "penaltiesJumpRaw") -> GT
        ("penaltiesJumpEffective", _) -> LT

        ("penalties", "place") -> GT
        ("penalties", "total") -> GT
        ("penalties", "essNotGoal") -> GT
        ("penalties", "penaltiesEssNotGoal") -> GT
        ("penalties", "jump") -> GT
        ("penalties", "penaltiesJumpRaw") -> GT
        ("penalties", "penaltiesJumpEffective") -> GT
        ("penalties", _) -> LT

        ("breakdown", "place") -> GT
        ("breakdown", "total") -> GT
        ("breakdown", "essNotGoal") -> GT
        ("breakdown", "penaltiesEssNotGoal") -> GT
        ("breakdown", "jump") -> GT
        ("breakdown", "penaltiesJumpRaw") -> GT
        ("breakdown", "penaltiesJumpEffective") -> GT
        ("breakdown", "penalties") -> GT
        ("breakdown", "penaltyReason") -> GT
        ("breakdown", _) -> LT

        ("velocity", "place") -> GT
        ("velocity", "total") -> GT
        ("velocity", "essNotGoal") -> GT
        ("velocity", "penaltiesEssNotGoal") -> GT
        ("velocity", "jump") -> GT
        ("velocity", "penaltiesJumpRaw") -> GT
        ("velocity", "penaltiesJumpEffective") -> GT
        ("velocity", "penalties") -> GT
        ("velocity", "penaltyReason") -> GT
        ("velocity", "breakdown") -> GT
        ("velocity", _) -> LT

        ("reach", "place") -> GT
        ("reach", "total") -> GT
        ("reach", "essNotGoal") -> GT
        ("reach", "penaltiesEssNotGoal") -> GT
        ("reach", "jump") -> GT
        ("reach", "penaltiesJumpRaw") -> GT
        ("reach", "penaltiesJumpEffective") -> GT
        ("reach", "penalties") -> GT
        ("reach", "penaltyReason") -> GT
        ("reach", "breakdown") -> GT
        ("reach", "velocity") -> GT
        ("reach", _) -> LT

        ("demeritFrac", "demeritPoint") -> LT
        ("demeritFrac", "demeritReset") -> LT
        ("demeritFrac", "penaltyReason") -> LT
        ("demeritFrac", "subtotal") -> LT
        ("demeritFrac", "landedMade") -> LT
        ("demeritFrac", "stoppedAlt") -> LT
        ("demeritFrac", _) -> GT

        ("demeritPoint", "demeritReset") -> LT
        ("demeritPoint", "penaltyReason") -> LT
        ("demeritPoint", "subtotal") -> LT
        ("demeritPoint", "landedMade") -> LT
        ("demeritPoint", "stoppedAlt") -> LT
        ("demeritPoint", _) -> GT

        ("demeritReset", "penaltyReason") -> LT
        ("demeritReset", "subtotal") -> LT
        ("demeritReset", "landedMade") -> LT
        ("demeritReset", "stoppedAlt") -> LT
        ("demeritReset", _) -> GT

        ("penaltyReason", "subtotal") -> LT
        ("penaltyReason", "landedMade") -> LT
        ("penaltyReason", "stoppedAlt") -> LT
        ("penaltyReason", _) -> GT

        ("subtotal", "landedMade") -> LT
        ("subtotal", "stoppedAlt") -> LT
        ("subtotal", _) -> GT

        ("landedMade", "stoppedAlt") -> LT
        ("landedMade", _) -> GT

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
        ("reachStats", _) -> LT

        ("effort", "reachStats") -> GT
        ("effort", _) -> LT

        ("distance", "reachStats") -> GT
        ("distance", "effort") -> GT
        ("distance", _) -> LT

        ("leading", "reachStats") -> GT
        ("leading", "effort") -> GT
        ("leading", "distance") -> GT
        ("leading", _) -> LT

        ("arrival", "reachStats") -> GT
        ("arrival", "effort") -> GT
        ("arrival", "distance") -> GT
        ("arrival", "leading") -> GT
        ("arrival", _) -> LT

        ("time", "reachStats") -> GT
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
