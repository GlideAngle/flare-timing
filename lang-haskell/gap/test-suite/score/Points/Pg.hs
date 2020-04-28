module Points.Pg (pgUnits) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Numeric.Natural (Natural)

import qualified Flight.Score as FS
import Flight.Score
    ( LaunchToStartPoints(..)
    , SitRep(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , DistancePoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , Points(..)
    , PointsReduced(..)
    , PointPenalty(..)
    , ReconcilePointErrors(..)
    , zeroPoints
    )

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

    , HU.testCase "Early start but with other negative reset" $
        -- TODO: Exclude construction of negative resets.
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyReset 1] [PenaltyReset (-2 :: Natural)] ptsAllOne)
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
