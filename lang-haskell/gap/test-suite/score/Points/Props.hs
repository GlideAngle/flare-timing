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
    , jumpTheGunPenalty
    )

import TestNewtypes

hgTaskPoints :: PtTest Hg -> Bool
hgTaskPoints (PtTest sitrep jumps others parts) =
    let expected = correct sitrep jumps others parts
        actual = (FS.taskPoints sitrep jumps others parts) <&> total
    in actual == expected

pgTaskPoints :: PtTest Pg -> Bool
pgTaskPoints (PtTest sitrep jumps others parts) =
    let expected = correct sitrep jumps others parts
        actual = (FS.taskPoints sitrep jumps others parts) <&> total
    in actual == expected

-- TODO: When base >= 4.11 use Data.Functor ((<&>))
(<&>) :: Either c a -> (a -> b) -> Either c b
(<&>) = flip (<$>)

correct
    :: forall a. (SitRep a)
    -> [PointPenalty]
    -> [PointPenalty]
    -> Points
    -> Either ReconcilePointErrors TaskPoints

correct
    NominalHg
    []
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = Right p
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        p = FS.applyPenalties others x

correct p@NominalHg jumps others _ =
    Left $ WAT_Nominal_Hg (p, jumps, others)

correct
    NoGoalHg
    []
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = Right p
    where
        x = TaskPoints (fromRational $ r + e + l + ((8 % 10) * (a + t)))
        p = FS.applyPenalties others x

correct p@NoGoalHg jumps _ _ =
    Left $ WAT_NoGoal_Hg (p, jumps)

correct p@(JumpedTooEarly (TooEarlyPoints ep)) [] others pts =
    correct p [PenaltyReset ep] others pts

correct p@JumpedTooEarly{} jumps@[PenaltyFraction{}] others _ =
    Left $ WAT_JumpedTooEarly (p, jumps, others)

correct p@JumpedTooEarly{} jumps@[PenaltyPoints{}] others _ =
    Left $ WAT_JumpedTooEarly (p, jumps, others)

correct
    p@(JumpedTooEarly (TooEarlyPoints ep))
    jumps@[PenaltyReset pr]
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if ep /= pr
           then Left $ EQ_JumpedTooEarly_Reset (p, jumps)
           else Right tp
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        tp = FS.applyPenalties (jumps ++ others) x

correct p@JumpedTooEarly{} jumps others _ =
    Left $ WAT_JumpedTooEarly (p, jumps, others)

correct p@(Jumped spp jtg) [] others pts =
    correct p [PenaltyPoints $ jumpTheGunPenalty spp jtg] others pts

correct p@(JumpedNoGoal spp jtg) [] others pts =
    correct p [PenaltyPoints $ jumpTheGunPenalty spp jtg] others pts

correct
    p@(Jumped spp jtg)
    jumps@[PenaltyPoints pJump]
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if jumpTheGunPenalty spp jtg /= pJump
           then Left $ EQ_Jumped_Point (p, jumps)
           else Right tp
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        tp = FS.applyPenalties (jumps ++ others) x

correct
    p@(JumpedNoGoal spp jtg)
    jumps@[PenaltyPoints pJump]
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    =
        if jumpTheGunPenalty spp jtg /= pJump
           then Left $ EQ_Jumped_Point (p, jumps)
           else Right tp
    where
        x = TaskPoints (fromRational $ r + e + l + ((8 % 10) * (t + a)))
        tp = FS.applyPenalties (jumps ++ others) x

correct p@Jumped{} jumps others _ =
    Left $ WAT_Jumped (p, jumps, others)

correct p@JumpedNoGoal{} jumps others _ =
    Left $ WAT_Jumped (p, jumps, others)

correct
    NominalPg
    []
    others
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        , time = TimePoints t
        }
    = Right p
    where
        x = TaskPoints (fromRational $ r + l + t)
        p = FS.applyPenalties others x

correct p@NominalPg jumps others _ =
    Left $ WAT_Nominal_Pg (p, jumps, others)

correct
    NoGoalPg
    []
    others
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        }
    = Right p
    where
        x = TaskPoints (fromRational $ r + l)
        p = FS.applyPenalties others x

correct p@NoGoalPg jumps _ _ =
    Left $ WAT_NoGoal_Pg (p, jumps)

correct p@(Early (LaunchToStartPoints lsp)) [] others pts =
    correct p [PenaltyReset lsp] others pts

correct p@Early{} jumps@[PenaltyFraction{}] _ _ =
    Left $ WAT_Early_Jump (p, jumps)

correct p@Early{} jumps@[PenaltyPoints{}] _ _ =
    Left $ WAT_Early_Jump (p, jumps)

correct
    p@(Early (LaunchToStartPoints lsp))
    jumps@[PenaltyReset pr]
    others
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        , time = TimePoints t
        }
    =
        if lsp /= pr
           then Left $ EQ_Early_Reset (p, jumps)
           else Right tp
    where
        x = TaskPoints (fromRational $ r + l + t)
        tp = FS.applyPenalties (jumps ++ others) x

correct p@Early{} jumps others _ =
    Left $ WAT_Early (p, jumps, others)
