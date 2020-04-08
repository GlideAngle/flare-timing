-- WARNING: This extension needs to be enabled at the definition site of a set
-- of record fields in order for them to be re-exported by a single module.
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/13352
{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Gap.Points
    ( LaunchToStartPoints(..)
    , TooEarlyPoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
    , Points(..)
    , PointsReduced(..)
    , zeroPoints
    , taskPoints
    , taskPointsSubtotal
    , availablePoints
    , jumpTheGunPenaltyHg
    , jumpTheGunPenaltyPg
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
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
import Flight.Gap.Time.Early (JumpTheGunLimit(..), JumpedTheGun(..), SecondsPerPoint(..))
import Flight.Gap.Penalty
    (PointPenalty(..), applyFractionalPenalties, applyPointPenalties, applyPenalties)

-- NOTE: Reset points are the final points awarded and so can be ints.
newtype LaunchToStartPoints = LaunchToStartPoints Int
    deriving (Eq, Ord, Show)
    deriving newtype (ToJSON, FromJSON)

newtype TooEarlyPoints = TooEarlyPoints Int
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

newtype NoGoal = NoGoal Bool deriving (Eq, Show)

data Hg = Hg deriving (Show)
data Pg = Pg deriving (Show)

data Penalty a where
    JumpedTooEarly :: TooEarlyPoints -> Penalty Hg

    Jumped
        :: SecondsPerPoint (Quantity Double [u| s |])
        -> JumpedTheGun (Quantity Double [u| s |])
        -> Penalty Hg

    JumpedNoGoal
        :: SecondsPerPoint (Quantity Double [u| s |])
        -> JumpedTheGun (Quantity Double [u| s |])
        -> Penalty Hg

    NoGoalHg :: Penalty Hg
    Early :: LaunchToStartPoints -> Penalty Pg
    NoGoalPg :: Penalty Pg

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
        TaskPoints . fromRational $ linear + diff + l + t + a

tallyPoints (Just (JumpedTooEarly (TooEarlyPoints p))) =
    const . TaskPoints $ fromIntegral p

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
        . fromRational
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
        . fromRational
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
        . fromRational
        $ linear + diff + l + (8 % 10) * (t + a)

tallyPoints (Just (Early (LaunchToStartPoints d))) =
    const . TaskPoints $ fromIntegral d

tallyPoints (Just NoGoalPg) =
    \Points
        { reach = LinearPoints linear
        , effort = DifficultyPoints diff
        , leading = LeadingPoints l
        } ->
        TaskPoints . fromRational $ linear + diff + l

jumpTheGunPenaltyHg
    :: TooEarlyPoints
    -> JumpTheGunLimit (Quantity Double [u| s |])
    -> SecondsPerPoint (Quantity Double [u| s |])
    -> JumpedTheGun (Quantity Double [u| s |])
    -> Either PointPenalty (Penalty Hg)
jumpTheGunPenaltyHg pts (JumpTheGunLimit secsMax) (SecondsPerPoint secs) (JumpedTheGun jump)
    | jump > secsMax = Right (JumpedTooEarly pts)
    | otherwise = Left $ let (MkQuantity penalty) = jump /: secs in PenaltyPoints penalty

jumpTheGunPenaltyPg
    :: LaunchToStartPoints
    -> JumpedTheGun (Quantity Double [u| s |])
    -> Maybe (Penalty Pg)
jumpTheGunPenaltyPg pts (JumpedTheGun jump)
    | jump > [u| 0 s |] = Just $ Early pts
    | otherwise = Nothing

jumpTheGun
    :: SecondsPerPoint (Quantity Double [u| s |])
    -> JumpedTheGun (Quantity Double [u| s |])
    -> TaskPoints
    -> TaskPoints
jumpTheGun (SecondsPerPoint secs) (JumpedTheGun jump) (TaskPoints pts) =
    let (MkQuantity penalty) = toRational' $ jump /: secs in
    TaskPoints $ max 0 (pts - fromRational penalty)

data PointsReduced =
    PointsReduced
        { subtotal :: TaskPoints
        , fracApplied :: TaskPoints
        , pointApplied :: TaskPoints
        , resetApplied :: TaskPoints
        , total :: TaskPoints
        , effectivePenalties :: [PointPenalty]
        , effectivePenaltiesJump :: [PointPenalty]
        }
        deriving (Eq, Show)

taskPoints
    :: forall a. Maybe (Penalty a) -- ^ Only applied if @isReset@ and @isTooEarly@.
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> PointsReduced
taskPoints p psJump ps points =
    PointsReduced
        { subtotal = subtotal
        , fracApplied =
            TaskPoints $ s - pointsF
        , pointApplied =
            TaskPoints $ pointsF - pointsFP
        , resetApplied =
            TaskPoints $ pointsFP - pointsR
        , total = total
        , effectivePenalties = qs'
        , effectivePenaltiesJump = psJump'
        }
    where
        subtotal@(TaskPoints s) = taskPointsSubtotal points
        TaskPoints reset = tallyPoints p points

        qs = psJump ++ ps
        withF@(TaskPoints pointsF) = applyFractionalPenalties qs subtotal
        (TaskPoints pointsFP) = applyPointPenalties qs withF

        (qs', psJump') =
            -- NOTE: If the penalty was a reset, change the penalties.
            maybe
                (qs, psJump)
                (\p' ->
                    if isReset p' && maybe False isTooEarly p
                        then
                            let pReset = PenaltyReset $ round reset in
                            (pReset : qs, pReset : psJump)
                        else
                            (qs, psJump))
                p

        TaskPoints pointsR = applyPenalties qs' subtotal
        total = applyPenalties qs' subtotal

isReset :: Penalty a -> Bool
isReset = \case
    JumpedTooEarly _ -> True
    Jumped _ _ -> False
    JumpedNoGoal _ _ -> False
    NoGoalHg -> False
    Early _ -> True
    NoGoalPg -> False

isTooEarly :: Penalty a -> Bool
isTooEarly = \case
    JumpedTooEarly _ -> True
    Jumped _ _ -> False
    JumpedNoGoal _ _ -> False
    NoGoalHg -> False
    Early _ -> True
    NoGoalPg -> False

taskPointsSubtotal :: Points -> TaskPoints
taskPointsSubtotal
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } =
    TaskPoints . fromRational $ r + e + l + t + a

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
