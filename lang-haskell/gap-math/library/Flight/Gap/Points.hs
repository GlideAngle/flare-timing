-- WARNING: This extension needs to be enabled at the definition site of a set
-- of record fields in order for them to be re-exported by a single module.
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/13352
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

module Flight.Gap.Points
    ( LaunchToStartPoints(..)
    , TooEarlyPoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , SitRep(..)
    , Points(..)
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

import Data.Refined (unrefined)
import Text.Printf (printf)
import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((/:), u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Points.Distance
    (DistancePoints(..), LinearPoints(..), DifficultyPoints(..))
import Flight.Gap.Points.Time (TimePoints(..))
import Flight.Gap.Points.Leading (LeadingPoints(..))
import Flight.Gap.Points.Arrival (ArrivalPoints(..))
import Flight.Gap.Points.Task (TaskPoints(..))
import "flight-gap-valid" Flight.Score (TaskValidity(..))
import "flight-gap-weight" Flight.Score (Weights(..))
import "flight-gap-weight" Flight.Score
    (DistanceWeight(..), ReachWeight(..), EffortWeight(..))
import "flight-gap-weight" Flight.Score (LeadingWeight(..))
import "flight-gap-weight" Flight.Score (ArrivalWeight(..))
import "flight-gap-weight" Flight.Score (TimeWeight(..))
import Flight.Gap.Time.Early (JumpTheGunLimit(..), JumpedTheGun(..), SecondsPerPoint(..))
import Flight.Gap.Penalty
    ( Add, Reset, PointsReduced(..), PenaltySeq(..), PenaltySeqs(..)
    , PointPenalty, TooEarlyPoints(..), LaunchToStartPoints(..)
    , applyPenalties, idSeq, nullSeqs, seqOnlyAdds, seqOnlyResets
    , mkAdd, mkReset
    , exAdd, exReset
    , effectiveAdd
    )

newtype NoGoal = NoGoal Bool deriving (Eq, Show)

data Hg = Hg deriving (Show)
data Pg = Pg deriving (Show)

data SitRep a where
    NominalHg :: SitRep Hg
    NoGoalHg :: SitRep Hg

    JumpedTooEarly :: TooEarlyPoints -> SitRep Hg

    Jumped
        :: TooEarlyPoints
        -> SecondsPerPoint (Quantity Double [u| s |])
        -> JumpedTheGun (Quantity Double [u| s |])
        -> SitRep Hg

    JumpedNoGoal
        :: TooEarlyPoints
        -> SecondsPerPoint (Quantity Double [u| s |])
        -> JumpedTheGun (Quantity Double [u| s |])
        -> SitRep Hg

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
jumpTheGunPenalty (SecondsPerPoint secs) (JumpedTheGun jump)
    -- WARNING: Some comps will have jump_the_gun_factor="0" as a sentinel value
    -- to turn off jump the gun.
    | secs > [u| 0 s |] = let (MkQuantity penalty) = jump /: secs in penalty
    | otherwise = 0

jumpTheGunSitRepHg
    :: TooEarlyPoints
    -> JumpTheGunLimit (Quantity Double [u| s |])
    -> SecondsPerPoint (Quantity Double [u| s |])
    -> JumpedTheGun (Quantity Double [u| s |])
    -> Either (PointPenalty Add) (SitRep Hg)
jumpTheGunSitRepHg pts (JumpTheGunLimit secsMax) spp jtg@(JumpedTheGun jump)
    | jump > secsMax = Right (JumpedTooEarly pts)
    | otherwise = Left . mkAdd . negate $ jumpTheGunPenalty spp jtg

jumpTheGunSitRepPg
    :: LaunchToStartPoints
    -> JumpedTheGun (Quantity Double [u| s |])
    -> Maybe (SitRep Pg)
jumpTheGunSitRepPg pts (JumpedTheGun jump)
    | jump > [u| 0 s |] = Just $ Early pts
    | otherwise = Nothing

data ReconcilePointErrors
    = EQ_JumpedTooEarly_Reset (SitRep Hg, PointPenalty Reset)
    | WAT_JumpedTooEarly (SitRep Hg, PenaltySeq, PenaltySeqs)
    | EQ_Early_Reset (SitRep Pg, PointPenalty Reset)
    | WAT_Early_Jump (SitRep Pg, PenaltySeq)
    | WAT_Early (SitRep Pg, PenaltySeq, PenaltySeqs)
    | WAT_Nominal_Hg (SitRep Hg, PenaltySeq, PenaltySeqs)
    | WAT_Nominal_Pg (SitRep Pg, PenaltySeq, PenaltySeqs)
    | WAT_NoGoal_Hg (SitRep Hg, PenaltySeq)
    | WAT_NoGoal_Pg (SitRep Pg, PenaltySeq)
    | WAT_Jumped (SitRep Hg, PenaltySeq, PenaltySeqs)
    | WAT_Jumped_Seconds_Per_Point (SitRep Hg)
    | EQ_Jumped_Point (SitRep Hg, PointPenalty Add)
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
    show (WAT_Jumped_Seconds_Per_Point e) =
        printf "Early HG with unexpected jump seconds per point %s" (show e)
    show (EQ_Jumped_Point e) =
        printf "Early HG with jump points /= seconds per point * seconds from %s" (show e)

reconcileEarlyHg
    :: SitRep Hg
    -> PenaltySeq -- ^ Penalties for jumping the gun.
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileEarlyHg p@(JumpedTooEarly (TooEarlyPoints tep)) j@((==) idSeq -> True) ps points =
    reconcileEarlyHg p j{reset = mkReset (Just $ unrefined tep)} ps points
reconcileEarlyHg p@(JumpedTooEarly (TooEarlyPoints tep)) js@(seqOnlyResets -> Just j) ps points =
    if | (Just $ unrefined tep) /= exReset j -> Left $ EQ_JumpedTooEarly_Reset (p, j)
       | otherwise -> Right $ reconcile p js ps points
reconcileEarlyHg p js ps _ =
    Left $ WAT_JumpedTooEarly (p, js, ps)

reconcileEarlyPg
    :: SitRep Pg
    -> PenaltySeq -- ^ Penalties for jumping the gun.
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileEarlyPg p@(Early (LaunchToStartPoints lsp)) j@((==) idSeq -> True) ps points =
    reconcileEarlyPg p j{reset = mkReset (Just $ unrefined lsp)} ps points
reconcileEarlyPg p@(Early (LaunchToStartPoints lsp)) js@(seqOnlyResets -> Just j) ps points =
    if | (Just $ unrefined lsp) /= exReset j -> Left $ EQ_Early_Reset (p, j)
       | otherwise -> Right $ reconcile p js ps points
reconcileEarlyPg p@Early{} js _ _ =
    Left $ WAT_Early_Jump (p, js)
reconcileEarlyPg p js ps _ =
    Left $ WAT_Early (p, js, ps)

reconcileNominal
    :: _
    -> SitRep b
    -> PenaltySeq -- ^ Penalties for jumping the gun.
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileNominal _ p j@((==) idSeq -> True) ps@((==) nullSeqs -> True) points =
    Right $ reconcile p j ps points
reconcileNominal _ p j@((==) idSeq -> True) ps points =
    Right $ reconcile p j ps points
reconcileNominal wat p js ps _ =
    Left $ wat (p, js, ps)

reconcileNoGoal
    :: _
    -> SitRep b
    -> PenaltySeq -- ^ Penalties for jumping the gun.
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
reconcileNoGoal _ p j@((==) idSeq -> True) ps@((==) nullSeqs -> True) points =
    Right $ reconcile p j ps points
reconcileNoGoal _ p j@((==) idSeq -> True) ps points =
    Right $ reconcile p j ps points
reconcileNoGoal wat p js _ _ =
    Left $ wat (p, js)

nonPositiveSecondsPerPoint :: SitRep a -> Bool
nonPositiveSecondsPerPoint (Jumped _ (SecondsPerPoint spp) _) = spp <= zero
nonPositiveSecondsPerPoint _ = False

-- | Apply penalties for jump the gun first and then the others because they
-- should not reduce the points below the score for minimum distance.
reconcileJumped
    :: SitRep Hg
    -> PenaltySeq -- ^ Penalties for jumping the gun.
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced

reconcileJumped p@(nonPositiveSecondsPerPoint -> True) _ _ _ =
    Left $ WAT_Jumped_Seconds_Per_Point p

-- NOTE: Convert jump-the-gun into a penalty.
reconcileJumped p@(Jumped _ spp jtg) j@((==) idSeq -> True) ps points =
    reconcileJumped p j{add = mkAdd . negate $ jumpTheGunPenalty spp jtg} ps points

-- NOTE: Convert jump-the-gun into a penalty.
reconcileJumped p@(JumpedNoGoal _ spp jtg) j@((==) idSeq -> True) ps points =
    reconcileJumped p j{add = mkAdd . negate $ jumpTheGunPenalty spp jtg} ps points

reconcileJumped p@(Jumped _ spp jtg) (seqOnlyAdds -> Just j) ps points =
    if (negate $ jumpTheGunPenalty spp jtg) /= exAdd j then
        Left $ EQ_Jumped_Point (p, j)
    else _reconcileJumped p (exAdd j) ps points

reconcileJumped p@(JumpedNoGoal _ spp jtg) (seqOnlyAdds -> Just j) ps points =
    if (negate $ jumpTheGunPenalty spp jtg) /= exAdd j then
        Left $ EQ_Jumped_Point (p, j)
    else _reconcileJumped p (exAdd j) ps points

reconcileJumped p j ps _ =
    Left $ WAT_Jumped (p, j, ps)

reconcile
    :: SitRep b
    -> PenaltySeq -- ^ Penalties for jumping the gun.
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> PointsReduced
reconcile p ((==) idSeq -> True) ((==) nullSeqs -> True) points =
    let x = tallySubtotal p points in
        PointsReduced
            { subtotal = x
            , mulApplied = 0
            , addApplied = 0
            , resetApplied = 0
            , total = x
            , effp = idSeq
            , effj = idSeq
            , rawj = nullSeqs
            }
reconcile p j ps points =
    let subtotal = tallySubtotal p points
        jMul = mul j
        jAdd = add j
        jReset = reset j

        pReduced =
            applyPenalties
                (muls ps)
                (adds ps)
                (resets ps)
                subtotal
        pjReduced =
            applyPenalties
                (jMul : muls ps)
                (jAdd : adds ps)
                (jReset : resets ps)
                subtotal
    in
        -- WARNING: Skip jump-the-gun penalties if they increase the total.
        if total pjReduced <= total pReduced
           then pjReduced{effj = PenaltySeq jMul jAdd jReset}
           else pReduced{effj = PenaltySeq jMul jAdd jReset}

tooEarly :: SitRep Hg -> Maybe TooEarlyPoints
tooEarly (Jumped tooE _ _) = Just tooE
tooEarly (JumpedNoGoal tooE _ _) = Just tooE
tooEarly _ = Nothing

_reconcileJumped
    :: SitRep Hg
    -> Double
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced

_reconcileJumped p@(tooEarly -> Just (TooEarlyPoints tooE)) pJump ps points =
    let subtotal = tallySubtotal p points

        j = mkAdd pJump
        PointsReduced{total = TaskPoints total'} = applyPenalties [] [j] [] subtotal

        -- NOTE: Limit the jump penalty so that points do not go below minimum.
        tooE' = fromIntegral $ unrefined tooE
        jLimited =
                j :
                if total' >= tooE'
                        then []
                        else [let pCorrection = tooE' - total' in mkAdd pCorrection]

        pReduced = applyPenalties (muls ps) (jLimited ++ adds ps) (resets ps) subtotal
    in
        Right
            pReduced
                { effj = (effj pReduced){ add = effectiveAdd jLimited }
                , rawj = (rawj pReduced){ adds = jLimited }
                }

_reconcileJumped p _ ps points =
    Right $ applyPenalties (muls ps) (adds ps) (resets ps) (tallySubtotal p points)

taskPoints
    :: SitRep b
    -> PenaltySeq -- ^ Penalties for jumping the gun.
    -> PenaltySeqs -- ^ Other penalties.
    -> Points
    -> Either ReconcilePointErrors PointsReduced
taskPoints s js ps points
    | JumpedTooEarly{} <- s = reconcileEarlyHg s js ps points
    | Early{} <- s = reconcileEarlyPg s js ps points
    | NominalHg <- s = reconcileNominal WAT_Nominal_Hg NominalHg js ps points
    | NominalPg <- s = reconcileNominal WAT_Nominal_Pg NominalPg js ps points
    | NoGoalHg <- s = reconcileNoGoal WAT_NoGoal_Hg NoGoalHg js ps points
    | NoGoalPg <- s = reconcileNoGoal WAT_NoGoal_Pg NoGoalPg js ps points

    -- WARNING: Some comps will have jump the gun settings where seconds per
    -- point is zero. This is a sentinel value for turning off jump the gun
    -- penalties.
    | (Jumped _ (SecondsPerPoint spp) _) <- s
    , spp > zero = reconcileJumped s js ps points

    | (JumpedNoGoal _ (SecondsPerPoint spp) _) <- s
    , spp > zero = reconcileJumped s js ps points

    | otherwise = Right $ reconcile s js ps points

isPg :: SitRep a -> Bool
isPg = \case
    NominalPg -> True
    Early _ -> True
    NoGoalPg -> True

    NominalHg -> False
    JumpedTooEarly _ -> False
    Jumped _ _ _ -> False
    JumpedNoGoal _ _ _ -> False
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
