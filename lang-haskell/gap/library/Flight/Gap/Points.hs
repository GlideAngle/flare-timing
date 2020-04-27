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
    , tallySubtotal
    , availablePointsPg
    , availablePointsHg
    , jumpTheGunPenaltyHg
    , jumpTheGunPenaltyPg
    ) where

import Text.Printf (printf)
import Data.Ratio ((%))
import Data.List (sort)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((/:), u)
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
    NoPenaltyHg :: Penalty Hg
    NoGoalHg :: Penalty Hg
    JumpedTooEarly :: TooEarlyPoints -> Penalty Hg
    Jumped :: SecondsPerPoint (Quantity Double [u| s |]) -> JumpedTheGun (Quantity Double [u| s |]) -> Penalty Hg
    JumpedNoGoal :: SecondsPerPoint (Quantity Double [u| s |]) -> JumpedTheGun (Quantity Double [u| s |]) -> Penalty Hg

    NoPenaltyPg :: Penalty Pg
    NoGoalPg :: Penalty Pg
    Early :: LaunchToStartPoints -> Penalty Pg

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

reconcileEarlyHg
    :: forall a. Penalty a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either String PointsReduced
reconcileEarlyHg p@(JumpedTooEarly (TooEarlyPoints tep)) [] ps points =
    reconcileEarlyHg p [PenaltyReset tep] ps points
reconcileEarlyHg p@(JumpedTooEarly (TooEarlyPoints tep)) psJump@[PenaltyReset r] ps points =
    if tep /= r then
        Left $ printf "Early HG with launch to start points /= reset but got %s" (show (p, psJump))
    else
        let subtotal@(TaskPoints s) = tallySubtotal p points

            withF@(TaskPoints pointsF) = applyFractionalPenalties ps subtotal
            (TaskPoints pointsFP) = applyPointPenalties ps withF

            total@(TaskPoints pointsR) = applyPenalties psJump subtotal
        in
            Right
            PointsReduced
                { subtotal = subtotal
                , fracApplied =
                    TaskPoints $ s - pointsF
                , pointApplied =
                    TaskPoints $ pointsF - pointsFP
                , resetApplied =
                    TaskPoints $ pointsFP - pointsR
                , total = total
                , effectivePenalties = ps ++ psJump
                , effectivePenaltiesJump = psJump
                }
reconcileEarlyHg p psJump ps _ =
    Left $ printf "Early HG with unexpected reconciliation of %s" (show (p, psJump, ps))

reconcileEarlyPg
    :: forall a. Penalty a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either String PointsReduced
reconcileEarlyPg p@(Early (LaunchToStartPoints lsp)) [] ps points =
    reconcileEarlyPg p [PenaltyReset lsp] ps points
reconcileEarlyPg p@(Early (LaunchToStartPoints lsp)) psJump@[PenaltyReset r] ps points =
    if lsp /= r then
        Left $ printf "Early PG with launch to start points /= reset but got %s" (show (p, psJump))
    else
        let subtotal@(TaskPoints s) = tallySubtotal p points

            -- NOTE: Take the smallest reset.
            qs = take 1 . sort $ psJump ++ filter (\case PenaltyReset{} -> True; _ -> False) ps
            withF@(TaskPoints pointsF) = applyFractionalPenalties qs subtotal
            (TaskPoints pointsFP) = applyPointPenalties qs withF

            TaskPoints pointsR = applyPenalties qs subtotal
            total = applyPenalties qs subtotal
        in
            Right
            PointsReduced
                { subtotal = subtotal
                , fracApplied =
                    TaskPoints $ s - pointsF
                , pointApplied =
                    TaskPoints $ pointsF - pointsFP
                , resetApplied =
                    TaskPoints $ pointsFP - pointsR
                , total = total
                , effectivePenalties = qs
                , effectivePenaltiesJump = psJump
                }
reconcileEarlyPg p@Early{} psJump _ _ =
    Left $ printf "Early PG with unexpected jump penalties %s" (show (p, psJump))
reconcileEarlyPg p psJump ps _ =
    Left $ printf "Early PG with unexpected reconciliation of %s" (show (p, psJump, ps))

reconcileNoPenalties
    :: forall a. Penalty a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either String PointsReduced
reconcileNoPenalties p [] [] points =
    let subtotal = tallySubtotal p points
    in
        Right
        PointsReduced
            { subtotal = subtotal
            , fracApplied =
                TaskPoints 0
            , pointApplied =
                TaskPoints 0
            , resetApplied =
                TaskPoints 0
            , total = subtotal
            , effectivePenalties = []
            , effectivePenaltiesJump = []
            }
reconcileNoPenalties p psJump ps _ =
    Left $ printf "Not no penalties %s" (show (p, psJump, ps))

reconcile
    :: forall a. Penalty a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either String PointsReduced
reconcile p psJump ps points =
    let subtotal@(TaskPoints s) = tallySubtotal p points

        qs = psJump ++ ps
        withF@(TaskPoints pointsF) = applyFractionalPenalties qs subtotal
        (TaskPoints pointsFP) = applyPointPenalties qs withF

        TaskPoints pointsR = applyPenalties qs subtotal
        total = applyPenalties qs subtotal
    in
        Right
        PointsReduced
            { subtotal = subtotal
            , fracApplied =
                TaskPoints $ s - pointsF
            , pointApplied =
                TaskPoints $ pointsF - pointsFP
            , resetApplied =
                TaskPoints $ pointsFP - pointsR
            , total = total
            , effectivePenalties = qs
            , effectivePenaltiesJump = psJump
            }

taskPoints
    :: forall a. Penalty a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either String PointsReduced
taskPoints p psJump ps points
    | JumpedTooEarly{} <- p = reconcileEarlyHg p psJump ps points
    | Early{} <- p = reconcileEarlyPg p psJump ps points
    | NoPenaltyHg <- p = reconcileNoPenalties p psJump ps points
    | NoPenaltyPg <- p = reconcileNoPenalties p psJump ps points
    | otherwise = reconcile p psJump ps points

isPg :: Penalty a -> Bool
isPg = \case
    NoPenaltyPg -> True
    Early _ -> True
    NoGoalPg -> True

    NoPenaltyHg -> False
    JumpedTooEarly _ -> False
    Jumped _ _ -> False
    JumpedNoGoal _ _ -> False
    NoGoalHg -> False

tallySubtotal :: Penalty a -> Points -> TaskPoints
tallySubtotal
    penalty
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        }
    | NoGoalPg <- penalty = TaskPoints . fromRational $ l + r
    | NoGoalHg <- penalty = TaskPoints . fromRational $ l + r + ((8 % 10) * (t + a))
    | JumpedNoGoal{} <- penalty = TaskPoints . fromRational $ l + r + ((8 % 10) * (t + a))
    | isPg penalty = TaskPoints . fromRational $ l + t + r
    | otherwise = TaskPoints . fromRational $ l + t + a + r + e

availablePointsPg :: TaskValidity -> Weights -> (Points, TaskPoints)
availablePointsPg (TaskValidity tv) Weights{..} =
    (pts, tallySubtotal NoPenaltyPg pts)
    where
        DistanceWeight dw = distance
        ReachWeight rw = reach
        LeadingWeight lw = leading
        TimeWeight tw = time
        k x = 1000 * tv * x

        pts =
            Points
                { reach = LinearPoints . k $ rw
                , effort = DifficultyPoints 0
                , distance = DistancePoints . k $ dw
                , leading = LeadingPoints . k $ lw
                , arrival = ArrivalPoints 0
                , time = TimePoints . k $ tw
                }

availablePointsHg :: TaskValidity -> Weights -> (Points, TaskPoints)
availablePointsHg (TaskValidity tv) Weights{..} =
    (pts, tallySubtotal NoPenaltyHg pts)
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
