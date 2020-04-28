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
    , SitRep(..)
    , Points(..)
    , PointsReduced(..)
    , ReconcilePointErrors(..)
    , zeroPoints
    , taskPoints
    , tallySubtotal
    , availablePointsPg
    , availablePointsHg
    , jumpTheGunSitRepHg
    , jumpTheGunSitRepPg
    , jumpTheGunPenalty
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
    ( PointPenalty(..), TooEarlyPoints(..), LaunchToStartPoints(..)
    , applyFractionalPenalties, applyPointPenalties, applyPenalties
    )

newtype NoGoal = NoGoal Bool deriving (Eq, Show)

data Hg = Hg deriving (Show)
data Pg = Pg deriving (Show)

data SitRep a where
    NominalHg :: SitRep Hg
    NoGoalHg :: SitRep Hg
    JumpedTooEarly :: TooEarlyPoints -> SitRep Hg
    Jumped :: SecondsPerPoint (Quantity Double [u| s |]) -> JumpedTheGun (Quantity Double [u| s |]) -> SitRep Hg
    JumpedNoGoal :: SecondsPerPoint (Quantity Double [u| s |]) -> JumpedTheGun (Quantity Double [u| s |]) -> SitRep Hg

    NominalPg :: SitRep Pg
    NoGoalPg :: SitRep Pg
    Early :: LaunchToStartPoints -> SitRep Pg

deriving instance Eq (SitRep a)
deriving instance Show (SitRep a)

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

jumpTheGunPenalty
    :: SecondsPerPoint (Quantity Double [u| s |])
    -> JumpedTheGun (Quantity Double [u| s |])
    -> Double
jumpTheGunPenalty (SecondsPerPoint secs) (JumpedTheGun jump) =
    let (MkQuantity penalty) = jump /: secs in penalty

jumpTheGunSitRepHg
    :: TooEarlyPoints
    -> JumpTheGunLimit (Quantity Double [u| s |])
    -> SecondsPerPoint (Quantity Double [u| s |])
    -> JumpedTheGun (Quantity Double [u| s |])
    -> Either PointPenalty (SitRep Hg)
jumpTheGunSitRepHg pts (JumpTheGunLimit secsMax) spp jtg@(JumpedTheGun jump)
    | jump > secsMax = Right (JumpedTooEarly pts)
    | otherwise = Left . PenaltyPoints $ jumpTheGunPenalty spp jtg

jumpTheGunSitRepPg
    :: LaunchToStartPoints
    -> JumpedTheGun (Quantity Double [u| s |])
    -> Maybe (SitRep Pg)
jumpTheGunSitRepPg pts (JumpedTheGun jump)
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

data ReconcilePointErrors
    = EQ_JumpedTooEarly_Reset (SitRep Hg, [PointPenalty])
    | WAT_JumpedTooEarly (SitRep Hg, [PointPenalty], [PointPenalty])
    | EQ_Early_Reset (SitRep Pg, [PointPenalty])
    | WAT_Early_Jump (SitRep Pg, [PointPenalty])
    | WAT_Early (SitRep Pg, [PointPenalty], [PointPenalty])
    | WAT_Nominal_Hg (SitRep Hg, [PointPenalty], [PointPenalty])
    | WAT_Nominal_Pg (SitRep Pg, [PointPenalty], [PointPenalty])
    | WAT_NoGoal_Hg (SitRep Hg, [PointPenalty])
    | WAT_NoGoal_Pg (SitRep Pg, [PointPenalty])
    | WAT_Jumped (SitRep Hg, [PointPenalty], [PointPenalty])
    | EQ_Jumped_Point (SitRep Hg, [PointPenalty])
    deriving Eq

instance Show ReconcilePointErrors where
    show (EQ_JumpedTooEarly_Reset e) =
        printf "Too early HG with launch to start points /= reset but got %s" (show e)
    show (WAT_JumpedTooEarly e) =
        printf "Too early HG with unexpected reconciliation of %s" (show e)
    show (EQ_Early_Reset e) =
        printf "Early PG with launch to start points /= reset but got %s" (show e)
    show (WAT_Early_Jump e) =
        printf "Early PG with unexpected jump penalties %s" (show e)
    show (WAT_Early e) =
        printf "Early PG with unexpected reconciliation of %s" (show e)
    show (WAT_Nominal_Hg e) =
        printf "Not no HG penalties %s" (show e)
    show (WAT_Nominal_Pg e) =
        printf "Not no PG penalties %s" (show e)
    show (WAT_NoGoal_Hg e) =
        printf "ESS but no goal HG with unexpected jump penalties %s" (show e)
    show (WAT_NoGoal_Pg e) =
        printf "ESS but no goal PG with unexpected jump penalties %s" (show e)
    show (WAT_Jumped e) =
        printf "Early HG with unexpected jump penalties of %s" (show e)
    show (EQ_Jumped_Point e) =
        printf "Early HG with jump points /= seconds per point * seconds from %s" (show e)

reconcileEarlyHg
    :: SitRep Hg
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileEarlyHg p@(JumpedTooEarly (TooEarlyPoints tep)) [] ps points =
    reconcileEarlyHg p [PenaltyReset tep] ps points
reconcileEarlyHg p@(JumpedTooEarly (TooEarlyPoints tep)) psJump@[PenaltyReset r] ps points =
    if | tep /= r -> Left $ EQ_JumpedTooEarly_Reset (p, psJump)
       | otherwise -> Right $ reconcile p psJump ps points
reconcileEarlyHg p psJump ps _ =
    Left $ WAT_JumpedTooEarly (p, psJump, ps)

reconcileEarlyPg
    :: SitRep Pg
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileEarlyPg p@(Early (LaunchToStartPoints lsp)) [] ps points =
    reconcileEarlyPg p [PenaltyReset lsp] ps points
reconcileEarlyPg p@(Early (LaunchToStartPoints lsp)) psJump@[PenaltyReset r] ps points =
    if | lsp /= r -> Left $ EQ_Early_Reset (p, psJump)
       | otherwise ->
            let subtotal@(TaskPoints s) = tallySubtotal p points

                withF@(TaskPoints pointsF) = applyFractionalPenalties ps subtotal
                withFP@(TaskPoints pointsFP) = applyPointPenalties ps withF

                resets = take 1 . sort $ psJump ++ filter (\case PenaltyReset{} -> True; _ -> False) ps
                total@(TaskPoints pointsR) = applyPenalties resets withFP
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
                    , effectivePenalties = resets
                    , effectivePenaltiesJump = psJump
                    }
reconcileEarlyPg p@Early{} psJump _ _ =
    Left $ WAT_Early_Jump (p, psJump)
reconcileEarlyPg p psJump ps _ =
    Left $ WAT_Early (p, psJump, ps)

reconcileNominal
    :: forall a. _
    -> SitRep a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileNominal _ p [] [] points =
    Right $ reconcile p [] [] points
reconcileNominal _ p [] ps points =
    Right $ reconcile p [] ps points
reconcileNominal wat p psJump ps _ =
    Left $ wat (p, psJump, ps)

reconcileNoGoal
    :: forall a. _
    -> SitRep a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileNoGoal _ p [] [] points =
    Right $ reconcile p [] [] points
reconcileNoGoal _ p [] ps points =
    Right $ reconcile p [] ps points
reconcileNoGoal wat p psJump _ _ =
    Left $ wat (p, psJump)

reconcileJumped
    :: SitRep Hg
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileJumped p@(Jumped spp jtg) [] ps points =
    reconcileJumped p [PenaltyPoints $ jumpTheGunPenalty spp jtg] ps points
reconcileJumped p@(JumpedNoGoal spp jtg) [] ps points =
    reconcileJumped p [PenaltyPoints $ jumpTheGunPenalty spp jtg] ps points
reconcileJumped p@(Jumped spp jtg) psJump@[PenaltyPoints pJump] ps points =
    if jumpTheGunPenalty spp jtg /= pJump then
        Left $ EQ_Jumped_Point (p, psJump)
    else _reconcileJumped p pJump ps points
reconcileJumped p@(JumpedNoGoal spp jtg) psJump@[PenaltyPoints pJump] ps points =
    if jumpTheGunPenalty spp jtg /= pJump then
        Left $ EQ_Jumped_Point (p, psJump)
    else _reconcileJumped p pJump ps points
reconcileJumped p psJump ps _ =
    Left $ WAT_Jumped (p, psJump, ps)

reconcile
    :: forall a. SitRep a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> PointsReduced
reconcile p [] [] points =
    let x = tallySubtotal p points in
        PointsReduced
            { subtotal = x
            , fracApplied = 0
            , pointApplied = 0
            , resetApplied = 0
            , total = x
            , effectivePenalties = []
            , effectivePenaltiesJump = []
            }
reconcile p psJump ps points =
    let subtotal@(TaskPoints s) = tallySubtotal p points

        withF@(TaskPoints pointsF) = applyFractionalPenalties ps subtotal
        withFP@(TaskPoints pointsFP) = applyPointPenalties ps withF

        resets = take 1 . sort $ psJump ++ filter (\case PenaltyReset{} -> True; _ -> False) ps
        total@(TaskPoints pointsR) = applyPenalties resets withFP
    in
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

_reconcileJumped
    :: SitRep Hg
    -> Double
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
_reconcileJumped p pJump ps points =
    let subtotal@(TaskPoints s) = tallySubtotal p points
        psJump = [PenaltyPoints pJump]
        withF@(TaskPoints pointsF) = applyFractionalPenalties ps subtotal
        total@(TaskPoints pointsFP) = applyPointPenalties (psJump ++ ps) withF
    in
        Right
        PointsReduced
            { subtotal = subtotal
            , fracApplied =
                TaskPoints $ s - pointsF
            , pointApplied =
                TaskPoints $ pointsF - pointsFP
            , resetApplied = 0
            , total = total
            , effectivePenalties = ps ++ psJump
            , effectivePenaltiesJump = psJump
            }

taskPoints
    :: forall a. SitRep a
    -> [PointPenalty] -- ^ Penalties for jumping the gun.
    -> [PointPenalty] -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
taskPoints s psJump ps points
    | JumpedTooEarly{} <- s = reconcileEarlyHg s psJump ps points
    | Early{} <- s = reconcileEarlyPg s psJump ps points
    | NominalHg <- s = reconcileNominal WAT_Nominal_Hg NominalHg psJump ps points
    | NominalPg <- s = reconcileNominal WAT_Nominal_Pg NominalPg psJump ps points
    | NoGoalHg <- s = reconcileNoGoal WAT_NoGoal_Hg NoGoalHg psJump ps points
    | NoGoalPg <- s = reconcileNoGoal WAT_NoGoal_Pg NoGoalPg psJump ps points
    | Jumped{} <- s = reconcileJumped s psJump ps points
    | JumpedNoGoal{} <- s = reconcileJumped s psJump ps points
    | otherwise = Right $ reconcile s psJump ps points

isPg :: SitRep a -> Bool
isPg = \case
    NominalPg -> True
    Early _ -> True
    NoGoalPg -> True

    NominalHg -> False
    JumpedTooEarly _ -> False
    Jumped _ _ -> False
    JumpedNoGoal _ _ -> False
    NoGoalHg -> False

tallySubtotal :: SitRep a -> Points -> TaskPoints
tallySubtotal
    s
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        }
    | NoGoalPg <- s = TaskPoints . fromRational $ l + r
    | NoGoalHg <- s = TaskPoints . fromRational $ l + r + ((8 % 10) * (t + a))
    | JumpedNoGoal{} <- s = TaskPoints . fromRational $ l + r + ((8 % 10) * (t + a))
    | isPg s = TaskPoints . fromRational $ l + t + r
    | otherwise = TaskPoints . fromRational $ l + t + a + r + e

availablePointsPg :: TaskValidity -> Weights -> (Points, TaskPoints)
availablePointsPg (TaskValidity tv) Weights{..} =
    (pts, tallySubtotal NominalPg pts)
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
    (pts, tallySubtotal NominalHg pts)
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
