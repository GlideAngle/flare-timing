{-# LANGUAGE ViewPatterns #-}

module Points.Props (hgTaskPoints, pgTaskPoints) where

import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
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
    , PointPenalty(..)
    , ReconcilePointErrors(..)
    , PenaltySeq(..), PenaltySeqs(..)
    , jumpTheGunPenalty
    , idSeq
    , addSeq, resetSeq
    , seqOnlyMuls, seqOnlyAdds, seqOnlyResets
    )

import TestNewtypes

hgTaskPoints :: PtTest Hg -> Bool
hgTaskPoints (PtTest sitrep js others parts) =
    let expected = correct sitrep js others parts
        actual = (FS.taskPoints sitrep js others parts) <&> total
    in actual == expected

pgTaskPoints :: PtTest Pg -> Bool
pgTaskPoints (PtTest sitrep js others parts) =
    let expected = correct sitrep js others parts
        actual = (FS.taskPoints sitrep js others parts) <&> total
    in actual == expected

-- TODO: When base >= 4.11 use Data.Functor ((<&>))
(<&>) :: Either c a -> (a -> b) -> Either c b
(<&>) = flip (<$>)

correct
    :: forall a. (SitRep a)
    -> PenaltySeq
    -> PenaltySeqs
    -> Points
    -> Either ReconcilePointErrors TaskPoints

correct
    NominalHg
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
correct p@NominalHg js others _ =
    Left $ WAT_Nominal_Hg (p, js, others)

correct
    NoGoalHg
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

correct p@NoGoalHg js _ _ =
    Left $ WAT_NoGoal_Hg (p, js)

correct p@(JumpedTooEarly (TooEarlyPoints ep)) ((==) idSeq -> True) others pts =
    correct p (resetSeq $ Just ep) others pts

correct p@JumpedTooEarly{} js@(seqOnlyMuls -> Just (PenaltyFraction{})) others _ =
    Left $ WAT_JumpedTooEarly (p, js, others)

correct p@JumpedTooEarly{} js@(seqOnlyAdds -> Just PenaltyPoints{}) others _ =
    Left $ WAT_JumpedTooEarly (p, js, others)

correct
    p@(JumpedTooEarly (TooEarlyPoints ep))
    (seqOnlyResets -> Just j@(PenaltyReset (Just pr)))
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if ep /= pr
           then Left $ EQ_JumpedTooEarly_Reset (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        tp = FS.applyPenalties muls adds (j : resets) x

correct p@JumpedTooEarly{} js others _ =
    Left $ WAT_JumpedTooEarly (p, js, others)

correct p@(Jumped spp jtg) ((==) idSeq -> True) others pts =
    correct p (addSeq $ jumpTheGunPenalty spp jtg) others pts

correct p@(JumpedNoGoal spp jtg) ((==) idSeq -> True) others pts =
    correct p (addSeq $ jumpTheGunPenalty spp jtg) others pts

correct
    p@(Jumped spp jtg)
    (seqOnlyAdds -> Just j@(PenaltyPoints pJump))
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if jumpTheGunPenalty spp jtg /= pJump
           then Left $ EQ_Jumped_Point (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        tp = FS.applyPenalties muls (j : adds) resets x

correct
    p@(JumpedNoGoal spp jtg)
    (seqOnlyAdds -> Just j@(PenaltyPoints pJump))
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if jumpTheGunPenalty spp jtg /= pJump
           then Left $ EQ_Jumped_Point (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + e + l + ((8 % 10) * (t + a)))
        tp = FS.applyPenalties muls (j : adds) resets x

correct p@Jumped{} js others _ =
    Left $ WAT_Jumped (p, js, others)

correct p@JumpedNoGoal{} js others _ =
    Left $ WAT_Jumped (p, js, others)

correct
    NominalPg
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

correct p@NominalPg js others _ =
    Left $ WAT_Nominal_Pg (p, js, others)

correct
    NoGoalPg
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

correct p@NoGoalPg js _ _ =
    Left $ WAT_NoGoal_Pg (p, js)

correct p@(Early (LaunchToStartPoints lsp)) ((==) idSeq -> True) others pts =
    correct p (resetSeq $ Just lsp) others pts

correct p@Early{} js@(seqOnlyMuls -> Just _) _ _ =
    Left $ WAT_Early_Jump (p, js)

correct p@Early{} js@(seqOnlyAdds -> Just _) _ _ =
    Left $ WAT_Early_Jump (p, js)

correct
    p@(Early (LaunchToStartPoints lsp))
    (seqOnlyResets -> Just j@(PenaltyReset (Just pr)))
    PenaltySeqs{muls, adds, resets}
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        , time = TimePoints t
        }
    =
        if lsp /= pr
           then Left $ EQ_Early_Reset (p, j)
           else Right $ total tp
    where
        x = TaskPoints (fromRational $ r + l + t)
        tp = FS.applyPenalties muls adds (j : resets) x

correct p@Early{} js others _ =
    Left $ WAT_Early (p, js, others)
