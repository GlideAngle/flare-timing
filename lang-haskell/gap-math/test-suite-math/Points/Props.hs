{-# LANGUAGE ViewPatterns #-}

module Points.Props (hgTaskPoints, pgTaskPoints) where

import Data.Refined (unrefined)
import Data.Ratio ((%))

import qualified "flight-gap-math" Flight.Score as FS
import "flight-gap-math" Flight.Score
    ( LaunchToStartPoints(..)
    , TooEarlyPoints(..)
    , Hg
    , Pg
    , SitRep(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , TooEarlyPoints(..)
    , Points(..)
    , PointsReduced(..)
    , ReconcilePointErrors(..)
    , GoalValidatedPoints
    , PenaltySeq(..), PenaltySeqs(..)
    , jumpTheGunPenalty
    , idSeq
    , addSeq, resetSeq
    , seqOnlyMuls, seqOnlyAdds, seqOnlyResets
    , exAdd, exReset
    )

import TestNewtypes
import Points.Round ((<&>))

hgTaskPoints :: PtTest Hg -> Bool
hgTaskPoints (PtTest sitrep eg js others parts) =
    let expected = correct sitrep eg js others parts
        actual = (FS.taskPoints sitrep eg js others parts) <&> total
    in actual == expected

pgTaskPoints :: PtTest Pg -> Bool
pgTaskPoints (PtTest sitrep eg js others parts) =
    let expected = correct sitrep eg js others parts
        actual = (FS.taskPoints sitrep eg js others parts) <&> total
    in actual == expected

correct
    :: forall a. (SitRep a)
    -> (GoalValidatedPoints -> PenaltySeq)
    -> PenaltySeq
    -> PenaltySeqs
    -> Points
    -> Either ReconcilePointErrors TaskPoints

correct
    NominalHg
    _eg
    ((==) idSeq -> True)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = Right $ total p
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        p = FS.applyPenalties muls adds resets x

correct p@NominalHg _eg js others _ =
    Left $ WAT_Nominal_Hg (p, js, others)

correct
    NoGoalHg
    _eg
    ((==) idSeq -> True)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = Right $ total p
    where
        x = TaskPoints (fromRational $ r + e + l + ((8 % 10) * (a + t)))
        p = FS.applyPenalties muls adds resets x

correct p@NoGoalHg _eg js _ _ =
    Left $ WAT_NoGoal_Hg (p, js)

correct p@(JumpedTooEarly (TooEarlyPoints ep)) eg ((==) idSeq -> True) others pts =
    correct p eg (resetSeq (Just $ unrefined ep)) others pts

correct p@JumpedTooEarly{} _eg js@(seqOnlyMuls -> Just _) others _ =
    Left $ WAT_JumpedTooEarly (p, js, others)

correct p@JumpedTooEarly{} _eg js@(seqOnlyAdds -> Just _) others _ =
    Left $ WAT_JumpedTooEarly (p, js, others)

correct
    p@(JumpedTooEarly (TooEarlyPoints ep))
    _eg
    (seqOnlyResets -> Just j)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if (Just $ unrefined ep) /= exReset j
           then Left $ EQ_JumpedTooEarly_Reset (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        tp = FS.applyPenalties muls adds (j : resets) x

correct p@JumpedTooEarly{} _eg js others _ =
    Left $ WAT_JumpedTooEarly (p, js, others)

correct p@(Jumped _ spp jtg) eg ((==) idSeq -> True) others pts =
    correct p eg (addSeq . negate $ jumpTheGunPenalty spp jtg) others pts

correct p@(JumpedNoGoal _ spp jtg) eg ((==) idSeq -> True) others pts =
    correct p eg (addSeq . negate $ jumpTheGunPenalty spp jtg) others pts

correct
    p@(Jumped _ spp jtg)
    _eg
    (seqOnlyAdds -> Just j)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if jumpTheGunPenalty spp jtg /= exAdd j
           then Left $ EQ_Jumped_Point (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        tp = FS.applyPenalties muls (j : adds) resets x

correct
    p@(JumpedNoGoal _ spp jtg)
    _eg
    (seqOnlyAdds -> Just j)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if jumpTheGunPenalty spp jtg /= exAdd j
           then Left $ EQ_Jumped_Point (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + e + l + ((8 % 10) * (t + a)))
        tp = FS.applyPenalties muls (j : adds) resets x

correct p@Jumped{} _eg js others _ =
    Left $ WAT_Jumped (p, js, others)

correct p@JumpedNoGoal{} _eg js others _ =
    Left $ WAT_Jumped (p, js, others)

correct
    NominalPg
    _eg
    ((==) idSeq -> True)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        , time = TimePoints t
        }
    = Right $ total p
    where
        x = TaskPoints (fromRational $ r + l + t)
        p = FS.applyPenalties muls adds resets x

correct p@NominalPg _eg js others _ =
    Left $ WAT_Nominal_Pg (p, js, others)

correct
    NoGoalPg
    _eg
    ((==) idSeq -> True)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        }
    = Right $ total p
    where
        x = TaskPoints (fromRational $ r + l)
        p = FS.applyPenalties muls adds resets x

correct p@NoGoalPg _eg js _ _ =
    Left $ WAT_NoGoal_Pg (p, js)

correct p@(Early (LaunchToStartPoints lsp)) eg ((==) idSeq -> True) others pts =
    correct p eg (resetSeq (Just $ unrefined lsp)) others pts

correct p@Early{} _eg js@(seqOnlyMuls -> Just _) _ _ =
    Left $ WAT_Early_Jump (p, js)

correct p@Early{} _eg js@(seqOnlyAdds -> Just _) _ _ =
    Left $ WAT_Early_Jump (p, js)

correct
    p@(Early (LaunchToStartPoints lsp))
    _eg
    (seqOnlyResets -> Just j)
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        , time = TimePoints t
        }
    =
        if (Just $ unrefined lsp) /= exReset j
           then Left $ EQ_Early_Reset (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + l + t)
        tp = FS.applyPenalties muls adds (j : resets) x

correct p@Early{} _eg js others _ =
    Left $ WAT_Early (p, js, others)
