-- WARNING: This extension needs to be enabled at the definition site of a set
-- of record fields in order for them to be re-exported by a single module.
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/13352
{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Gap.Points
    ( LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
    , PointPenalty(..)
    , Points(..)
    , zeroPoints
    , taskPoints
    , applyPointPenalties
    , availablePoints
    , jumpTheGunPenalty
    ) where

import Data.Ratio ((%))
import Data.List (partition, foldl')
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), Options(..), SumEncoding(..)
    , genericToJSON, genericParseJSON, defaultOptions
    )
import Data.UnitsOfMeasure ((/:), u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Points.Distance
    (DistancePoints(..), LinearPoints(..), DifficultyPoints(..))
import Flight.Gap.Points.Time (TimePoints(..))
import Flight.Gap.Points.Leading (LeadingPoints(..))
import Flight.Gap.Points.Arrival (ArrivalPoints(..))
import Flight.Gap.Points.Task (TaskPoints(..))
import Flight.Gap.Validity.Task (TaskValidity(..))
import Flight.Gap.Weighting (Weights(..))
import Flight.Gap.Weight.Distance
    (DistanceWeight(..), ReachWeight(..), EffortWeight(..))
import Flight.Gap.Weight.Leading (LeadingWeight(..))
import Flight.Gap.Weight.Arrival (ArrivalWeight(..))
import Flight.Gap.Weight.Time (TimeWeight(..))
import Flight.Gap.Time.Early (JumpedTheGun(..), SecondsPerPoint(..))

newtype LaunchToSssPoints = LaunchToSssPoints Rational deriving (Eq, Show)
newtype MinimumDistancePoints = MinimumDistancePoints Rational deriving (Eq, Show)

newtype NoGoal = NoGoal Bool deriving (Eq, Show)

data Hg = Hg deriving (Show)
data Pg = Pg deriving (Show)

data Penalty a where
    JumpedTooEarly :: MinimumDistancePoints -> Penalty Hg

    Jumped
        :: SecondsPerPoint (Quantity Double [u| s |])
        -> JumpedTheGun (Quantity Double [u| s |])
        -> Penalty Hg

    JumpedNoGoal
        :: SecondsPerPoint (Quantity Double [u| s |])
        -> JumpedTheGun (Quantity Double [u| s |])
        -> Penalty Hg

    NoGoalHg :: Penalty Hg
    Early :: LaunchToSssPoints -> Penalty Pg
    NoGoalPg :: Penalty Pg

data PointPenalty
    = PenaltyPoints Double
    | PenaltyFraction Double
    deriving (Eq, Ord, Show, Generic)

pointPenaltyOptions :: Options
pointPenaltyOptions =
    defaultOptions
        { sumEncoding = ObjectWithSingleField
        , constructorTagModifier = \case
            "PenaltyPoints" -> "penalty-points"
            "PenaltyFraction" -> "penalty-fraction"
            s -> s
        }

instance ToJSON PointPenalty where
    toJSON = genericToJSON pointPenaltyOptions

instance FromJSON PointPenalty where
    parseJSON = genericParseJSON pointPenaltyOptions

deriving instance Eq (Penalty a)
deriving instance Show (Penalty a)

data Points =
    Points
        { reach :: LinearPoints
        , effort :: DifficultyPoints
        , distance :: DistancePoints
        , leading :: LeadingPoints
        , arrival :: ArrivalPoints
        , time :: TimePoints
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type TaskPointTally = Points -> TaskPoints

zeroPoints :: Points
zeroPoints =
    Points
        { reach = LinearPoints 0
        , effort = DifficultyPoints 0
        , distance = DistancePoints 0
        , leading = LeadingPoints 0
        , time = TimePoints 0
        , arrival = ArrivalPoints 0
        }

tallyPoints :: forall a. Maybe (Penalty a) -> TaskPointTally

tallyPoints Nothing =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        TaskPoints $ linear + diff + l + t + a

tallyPoints (Just (JumpedTooEarly (MinimumDistancePoints p))) =
    const $ TaskPoints p

tallyPoints (Just (JumpedNoGoal secs jump)) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        jumpTheGun secs jump
        . TaskPoints
        $ linear + diff + l + (8 % 10) * (t + a)

tallyPoints (Just (Jumped secs jump)) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        jumpTheGun secs jump
        . TaskPoints
        $ linear + diff + l + t + a

tallyPoints (Just NoGoalHg) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } ->
        TaskPoints
        $ linear + diff + l + (8 % 10) * (t + a)

tallyPoints (Just (Early (LaunchToSssPoints d))) =
    const $ TaskPoints d

tallyPoints (Just NoGoalPg) =
    \Points
        { reach = LinearPoints linear 
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        } ->
        TaskPoints $ linear + diff + l

jumpTheGunPenalty
    :: SecondsPerPoint (Quantity Double [u| s |])
    -> JumpedTheGun (Quantity Double [u| s |])
    -> PointPenalty
jumpTheGunPenalty (SecondsPerPoint secs) (JumpedTheGun jump) =
    let (MkQuantity penalty) = jump /: secs in
    PenaltyPoints penalty

jumpTheGun
    :: SecondsPerPoint (Quantity Double [u| s |])
    -> JumpedTheGun (Quantity Double [u| s |])
    -> TaskPoints
    -> TaskPoints
jumpTheGun (SecondsPerPoint secs) (JumpedTheGun jump) (TaskPoints pts) =
    let (MkQuantity penalty) = toRational' $ jump /: secs in
    TaskPoints $ max 0 (pts - penalty)

taskPoints :: forall a. Maybe (Penalty a) -> Points -> TaskPoints
taskPoints = tallyPoints

-- | Applies the penalties, fractional ones before absolute ones.
applyPointPenalties :: [PointPenalty] -> TaskPoints -> TaskPoints
applyPointPenalties xs ps =
    foldl' f (foldl' f ps fracs) points
    where
        f = applyPointPenalty
        (fracs, points) =
            partition
                (\case
                    PenaltyFraction _ -> True
                    PenaltyPoints _ -> False)
                xs

applyPointPenalty :: TaskPoints -> PointPenalty -> TaskPoints
applyPointPenalty (TaskPoints p) (PenaltyPoints n) =
    TaskPoints . max 0 $ p - (toRational n)
applyPointPenalty (TaskPoints p) (PenaltyFraction n) =
    TaskPoints . max 0 $ p - p * (toRational n)

availablePoints :: TaskValidity -> Weights -> (Points, TaskPoints)
availablePoints (TaskValidity tv) Weights{..} =
    (pts, tallyPoints Nothing pts)
    where
        DistanceWeight dw = distance
        ReachWeight rw = reach
        EffortWeight ew = effort
        LeadingWeight lw = leading
        ArrivalWeight aw = arrival
        TimeWeight tw = time
        k x = 1000 * tv * x

        pts =
            Points
                { reach = LinearPoints . k $ rw
                , effort = DifficultyPoints . k $ ew
                , distance = DistancePoints . k $ dw
                , leading = LeadingPoints . k $ lw
                , arrival = ArrivalPoints . k $ aw
                , time = TimePoints . k $ tw
                }
