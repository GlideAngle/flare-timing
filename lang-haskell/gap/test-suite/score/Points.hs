module Points (hgUnits, pgUnits, hgTaskPoints, pgTaskPoints) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Either (lefts, rights)
import Data.UnitsOfMeasure (u)

import qualified Flight.Score as FS
import Flight.Score
    ( LaunchToStartPoints(..)
    , TooEarlyPoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , JumpTheGunLimit(..)
    , Hg
    , Pg
    , SitRep(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , DistancePoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , TooEarlyPoints(..)
    , Points(..)
    , PointsReduced(..)
    , PointPenalty(..)
    , ReconcilePointErrors(..)
    , zeroPoints
    , jumpTheGunPenalty
    )

import TestNewtypes

-- TODO: When base >= 4.11 use Data.Functor ((<&>))
(<&>) :: Either c a -> (a -> b) -> Either c b
(<&>) = flip (<$>)

ptsAllOne :: Points
ptsAllOne =
    Points
        { reach = LinearPoints 1
        , effort = DifficultyPoints 1
        , distance = DistancePoints 2
        , leading = LeadingPoints 1
        , arrival = ArrivalPoints 1
        , time = TimePoints 1
        }

hgUnits :: TestTree
hgUnits = testGroup "HG Points"
    [ HU.testCase "No penalties and no points = zero task points" $
        ((FS.taskPoints NominalHg [] [] zeroPoints) <&> total) @?= Right (TaskPoints 0)

    , HU.testCase "No penalties = sum of reach, effort, leading, time & arrival points" $
        (FS.taskPoints NominalHg [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 5
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 5
                    , effectivePenalties = []
                    , effectivePenaltiesJump = []
                    }

    , HU.testCase "No penalties, ESS but no goal = sum of reach, effort, leading, 80% of time & arrival points" $
        (FS.taskPoints NoGoalHg [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3.6
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 3.6
                    , effectivePenalties = []
                    , effectivePenaltiesJump = []
                    }

    , HU.testCase "No penalties, ESS but no goal with jump penalty points" $
        (FS.taskPoints NoGoalHg [PenaltyPoints 0] [] ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Hg (NoGoalHg,[PenaltyPoints 0]))

    , HU.testCase "No penalties, ESS but no goal with jump penalty fraction" $
        (FS.taskPoints NoGoalHg [PenaltyFraction 0] [] ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Hg (NoGoalHg,[PenaltyFraction 0]))

    , HU.testCase "Way too early start = minimum distance points only" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints 1) [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 5
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 4
                    , total = TaskPoints 1
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start, ESS but no goal = ESS no goal points minus jump the gun penalty" $
        let jump = JumpedTheGun [u| 2 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints 1) limit secs jump in [x]
        in
            (FS.taskPoints (JumpedNoGoal secs jump) jumps [] ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3.6
                        , fracApplied = TaskPoints 0
                        , pointApplied = TaskPoints 2
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 1.6
                        , effectivePenalties = [PenaltyPoints 2]
                        , effectivePenaltiesJump = [PenaltyPoints 2]
                        }

    , HU.testCase "Early start, ESS but no goal with zero jump penalty" $
        let jump = JumpedTheGun [u| 2 s |]
            secs = SecondsPerPoint [u| 1 s |]
        in
            (FS.taskPoints (JumpedNoGoal secs jump) [PenaltyPoints 0] [] ptsAllOne)
                @?=
                    (Left $ EQ_Jumped_Point (JumpedNoGoal (SecondsPerPoint [u| 1.0 s |]) (JumpedTheGun [u| 2.0 s |]),[PenaltyPoints 0.0]))

    , HU.testCase "Early start = full points minus jump the gun penalty" $
        let jump = JumpedTheGun [u| 2 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints 1) limit secs jump in [x]
        in
            (FS.taskPoints (Jumped secs jump) jumps [] ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , fracApplied = TaskPoints 0
                        , pointApplied = TaskPoints 2
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 3
                        , effectivePenalties = [PenaltyPoints 2]
                        , effectivePenaltiesJump = [PenaltyPoints 2]
                        }

    , HU.testCase "Early start and no goal = full points minus jump the gun penalty" $
        let jump = JumpedTheGun [u| 2 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints 1) limit secs jump in [x]
        in
            (FS.taskPoints (JumpedNoGoal secs jump) jumps [] ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3.6
                        , fracApplied = TaskPoints 0
                        , pointApplied = TaskPoints 2
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 1.6
                        , effectivePenalties = [PenaltyPoints 2]
                        , effectivePenaltiesJump = [PenaltyPoints 2]
                        }

    , HU.testCase "Very early start = full points minus jump the gun penalty resulting in no less than zero" $
        let jump = JumpedTheGun [u| 3 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints 1) limit secs jump in [x]
        in
            (FS.taskPoints (Jumped secs jump) jumps [] ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , fracApplied = TaskPoints 0
                        , pointApplied = TaskPoints 3
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 2
                        , effectivePenalties = [PenaltyPoints 3]
                        , effectivePenaltiesJump = [PenaltyPoints 3]
                        }

    , HU.testCase "Too early start = points for minimum distance" $
        let jump = JumpedTheGun [u| 4 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            eitherPenalties :: [Either PointPenalty (SitRep Hg)]
            eitherPenalties =
                return $ FS.jumpTheGunSitRepHg (TooEarlyPoints 1) limit secs jump

            jumpDemerits = lefts eitherPenalties
            jumpReset = fromMaybe NominalHg . listToMaybe $ rights eitherPenalties
        in
            (FS.taskPoints
                jumpReset
                jumpDemerits
                []
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , fracApplied = TaskPoints 0
                        , pointApplied = TaskPoints 0
                        , resetApplied = TaskPoints 4
                        , total = TaskPoints 1
                        , effectivePenalties = [PenaltyReset 1]
                        , effectivePenaltiesJump = [PenaltyReset 1]
                        }

    , HU.testCase "Early start = error on wrong reset applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints 1) [PenaltyReset 2] [] ptsAllOne)
            @?=
                (Left $ EQ_JumpedTooEarly_Reset (JumpedTooEarly (TooEarlyPoints 1),[PenaltyReset 2]))

    , HU.testCase "Early start = error on wrong type of penalty, a point penalty, applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints 1) [PenaltyPoints 1] [] ptsAllOne)
            @?=
                (Left $ WAT_JumpedTooEarly (JumpedTooEarly (TooEarlyPoints 1),[PenaltyPoints 1.0],[]))

    , HU.testCase "Early start = error on wrong type of penalty, a fraction penalty, applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints 1) [PenaltyFraction 0.5] [] ptsAllOne)
            @?=
                (Left $ WAT_JumpedTooEarly (JumpedTooEarly (TooEarlyPoints 1),[PenaltyFraction 0.5],[]))
    ]

pgUnits :: TestTree
pgUnits = testGroup "PG Points"
    [ HU.testCase "No penalties and no points = zero task points" $
        ((FS.taskPoints NominalPg [] [] zeroPoints) <&> total) @?= Right (TaskPoints 0)

    , HU.testCase "No penalties = sum of reach, leading, time points, ignoring effort and arrival" $
        (FS.taskPoints NominalPg [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 3
                    , effectivePenalties = []
                    , effectivePenaltiesJump = []
                    }

    , HU.testCase "No penalties, ESS but no goal = sum of reach, effort, leading, 80% of time & arrival points" $
        (FS.taskPoints NoGoalPg [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 2
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 2
                    , effectivePenalties = []
                    , effectivePenaltiesJump = []
                    }

    , HU.testCase "No penalties, ESS but no goal with jump penalty points" $
        (FS.taskPoints NoGoalPg [PenaltyPoints 0] [] ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Pg (NoGoalPg,[PenaltyPoints 0]))

    , HU.testCase "No penalties, ESS but no goal with jump penalty fraction" $
        (FS.taskPoints NoGoalPg [PenaltyFraction 0] [] ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Pg (NoGoalPg,[PenaltyFraction 0]))

    , HU.testCase "Applying too many penalty points = zero" $
        (FS.taskPoints NominalPg [] [PenaltyPoints 4] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 3
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0
                    , effectivePenalties = [PenaltyPoints 4]
                    , effectivePenaltiesJump = []
                    }

    , HU.testCase "Applying too many fraction points = zero" $
        (FS.taskPoints NominalPg [] [PenaltyFraction 2] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 3
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0
                    , effectivePenalties = [PenaltyFraction 2]
                    , effectivePenaltiesJump = []
                    }

    , HU.testCase "Applying negative fraction points increases score" $
        (FS.taskPoints NominalPg [] [PenaltyFraction (-1)] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints (-3)
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 6
                    , effectivePenalties = [PenaltyFraction (-1)]
                    , effectivePenaltiesJump = []
                    }

    , HU.testCase "Applying jump penalty points with nominal situation report" $
        (FS.taskPoints NominalPg [PenaltyFraction 0] [] ptsAllOne)
            @?=
                (Left $ WAT_Nominal_Pg (NominalPg,[PenaltyFraction 0.0],[]))

    , HU.testCase "Applying jump penalty fraction with nominal situation report" $
        (FS.taskPoints NominalPg [PenaltyFraction 0] [] ptsAllOne)
            @?=
                (Left $ WAT_Nominal_Pg (NominalPg,[PenaltyFraction 0.0],[]))

    , HU.testCase "Early start = distance to start points only, ignoring effort and arrival, jump reset generated" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start = distance to start points only, ignoring effort and arrival, jump reset applied" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyReset 1] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start = other score-lowering reset can't raise the total" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyReset 1] [PenaltyPoints 1, PenaltyFraction 0.5] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 1.5
                    , pointApplied = TaskPoints 1
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0.5
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start = other non-reset score-raising penalties don't change total" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyReset 1] [PenaltyPoints (-1), PenaltyFraction (-1)] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints (- 3)
                    , pointApplied = TaskPoints (- 1)
                    , resetApplied = TaskPoints 6
                    , total = TaskPoints 1
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start = taking the smallest reset penalty from jump and other sets" $
        (FS.taskPoints (Early $ LaunchToStartPoints 2) [PenaltyReset 2] [PenaltyReset 1] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 2]
                    }

    , HU.testCase "Early start = error on wrong reset applied" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyReset 2] [] ptsAllOne)
            @?=
                (Left $ EQ_Early_Reset (Early (LaunchToStartPoints 1),[PenaltyReset 2]))

    , HU.testCase "Early start = error on wrong type of jump penalty, a point penalty, applied" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyPoints 1] [] ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early (LaunchToStartPoints 1),[PenaltyPoints 1.0]))

    , HU.testCase "Early start = error on wrong type of penalty, a fraction penalty, applied" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyFraction 0.5] [] ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early (LaunchToStartPoints 1),[PenaltyFraction 0.5]))

    , HU.testCase "Early start = error on wrong type of jump penalty, a point penalty, applied, with other reset" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyPoints 0] [PenaltyReset 0] ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early (LaunchToStartPoints 1),[PenaltyPoints 0]))

    , HU.testCase "Early start = error on wrong type of penalty, a fraction penalty, applied, with other resete" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyFraction 0.5] [PenaltyReset 0] ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early (LaunchToStartPoints 1),[PenaltyFraction 0.5]))

    , HU.testCase "Early start tolerates no explicit jump penalty" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start tolerates other reset to zero" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [] [PenaltyReset 0] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 3
                    , total = TaskPoints 0
                    , effectivePenalties = [PenaltyReset 0]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start still applies other penalty of fraction 1" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [] [PenaltyFraction 1] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , fracApplied = TaskPoints 3
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0
                    , effectivePenalties = [PenaltyReset 1]
                    , effectivePenaltiesJump = [PenaltyReset 1]
                    }

    , HU.testCase "Early start still applies other penalty of fraction 1" $
        (FS.taskPoints (Early $ LaunchToStartPoints 7) [PenaltyReset 11] [PenaltyReset 1] ptsAllOne)
            @?=
                (Left $ EQ_Early_Reset (Early (LaunchToStartPoints 7),[PenaltyReset 11]))
    ]

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

hgTaskPoints :: PtTest Hg -> Bool
hgTaskPoints (PtTest (penalty, jumps, others, parts)) =
    let expected = correct penalty jumps others parts
        actual = (FS.taskPoints penalty jumps others parts) <&> total
    in actual == expected

pgTaskPoints :: PtTest Pg -> Bool
pgTaskPoints (PtTest (penalty, jumps, others, parts)) =
    let expected = correct penalty jumps others parts
        actual = (FS.taskPoints penalty jumps others parts) <&> total
    in actual == expected
