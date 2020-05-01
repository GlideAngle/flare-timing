module Points.Pg (pgUnits) where

import Data.Refined (assumeProp, refined)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

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
    , ReconcilePointErrors(..)
    , PenaltySeqs(..)
    , zeroPoints
    , idSeq, nullSeqs
    , mulSeq, addSeq, resetSeq
    , mkMul, mkAdd, mkReset
    )

-- TODO: When base >= 4.11 use Data.Functor ((<&>))
(<&>) :: Either c a -> (a -> b) -> Either c b
(<&>) = flip (<$>)

launchToStart1, launchToStart2, launchToStart7 :: LaunchToStartPoints
launchToStart1 = LaunchToStartPoints . assumeProp $ refined 1
launchToStart2 = LaunchToStartPoints . assumeProp $ refined 2
launchToStart7 = LaunchToStartPoints . assumeProp $ refined 7

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
    [ HU.testCase "✓ No penalties and no points = zero task points" $
        ((FS.taskPoints NominalPg idSeq nullSeqs zeroPoints) <&> total) @?= Right (TaskPoints 0)

    , HU.testCase "✓ No penalties = sum of reach, leading, time points, ignoring effort and arrival" $
        (FS.taskPoints NominalPg idSeq nullSeqs ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 3
                    , effp = idSeq
                    , effj = idSeq
                    }

    , HU.testCase "✓ No penalties, ESS but no goal = sum of reach, effort, leading, 80% of time & arrival points" $
        (FS.taskPoints NoGoalPg idSeq nullSeqs ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 2
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 2
                    , effp = idSeq
                    , effj = idSeq
                    }

    , HU.testCase "✘ No penalties, ESS but no goal with jump penalty points == 0" $
        (FS.taskPoints NoGoalPg (addSeq 0) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Pg (NoGoalPg, idSeq))

    , HU.testCase "✘ No penalties, ESS but no goal with jump penalty fraction == 1" $
        (FS.taskPoints NoGoalPg (mulSeq 1) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Pg (NoGoalPg, idSeq))

    , HU.testCase "✓ Applying too many penalty points = zero" $
        (FS.taskPoints NominalPg idSeq nullSeqs{adds = [mkAdd 4]} ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 3
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0
                    , effp = addSeq 4
                    , effj = idSeq
                    }

    , HU.testCase "✓ Applying too many fraction points = zero" $
        (FS.taskPoints NominalPg idSeq nullSeqs{muls = [mkMul 2]} ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 3
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0
                    , effp = mulSeq 2
                    , effj = idSeq
                    }

    , HU.testCase "✓ Applying negative fraction points increases score" $
        (FS.taskPoints NominalPg idSeq nullSeqs{muls = [mkMul (-1)]} ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints (-3)
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 6
                    , effp = mulSeq (-1)
                    , effj = idSeq
                    }

    , HU.testCase "✘ Applying jump penalty points with nominal situation report" $
        (FS.taskPoints NominalPg (mulSeq 0) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_Nominal_Pg (NominalPg, idSeq, nullSeqs))

    , HU.testCase "✘ Applying jump penalty fraction with nominal situation report" $
        (FS.taskPoints NominalPg (mulSeq 0) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_Nominal_Pg (NominalPg, idSeq, nullSeqs))

    , HU.testCase "✓ Early start = distance to start points only, ignoring effort and arrival, jump reset generated" $
        (FS.taskPoints (Early launchToStart1) idSeq nullSeqs ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effp = resetSeq $ Just 1
                    , effj = resetSeq $ Just 1
                    }

    , HU.testCase "✓ Early start = distance to start points only, ignoring effort and arrival, jump reset applied" $
        (FS.taskPoints
            (Early launchToStart1)
            (resetSeq $ Just 1)
            nullSeqs
            ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effp = resetSeq $ Just 1
                    , effj = resetSeq $ Just 1
                    }

    , HU.testCase "✓ Early start = other score-lowering reset can't raise the total" $
        (FS.taskPoints (Early launchToStart1)
            (resetSeq $ Just 1)
            nullSeqs{adds = [mkAdd 1], muls = [mkMul 0.5]}
            ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 1.5
                    , addApplied = TaskPoints 1
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0.5
                    , effp = resetSeq $ Just 1
                    , effj = resetSeq $ Just 1
                    }

    , HU.testCase "✓ Early start = other non-reset score-raising penalties don't change total" $
        (FS.taskPoints
            (Early launchToStart1)
            (resetSeq (Just 1))
            nullSeqs{adds = [mkAdd (-1)], muls = [mkMul (-1)]}
            ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints (- 3)
                    , addApplied = TaskPoints (- 1)
                    , resetApplied = TaskPoints 6
                    , total = TaskPoints 1
                    , effp = resetSeq $ Just 1
                    , effj = resetSeq $ Just 1
                    }

    , HU.testCase "✓ Early start = taking the smallest reset penalty from jump and other sets" $
        (FS.taskPoints
            (Early launchToStart2)
            (resetSeq $ Just 2)
            nullSeqs{resets = [mkReset $ Just 1]}
            ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effp = resetSeq $ Just 1
                    , effj = resetSeq $ Just 2
                    }

    , HU.testCase "✘ Early start = error on wrong reset applied" $
        (FS.taskPoints (Early launchToStart1) (resetSeq $ Just 2) nullSeqs ptsAllOne)
            @?=
                (Left $ EQ_Early_Reset (Early launchToStart1, mkReset $ Just 2))

    , HU.testCase "✘ Early start = error on wrong type of jump penalty, a point penalty, applied" $
        (FS.taskPoints (Early launchToStart1) (addSeq 1) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early launchToStart1, addSeq 1))

    , HU.testCase "✘ Early start = error on wrong type of penalty, a fraction penalty, applied" $
        (FS.taskPoints (Early launchToStart1) (mulSeq 0.5) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early launchToStart1, mulSeq 0.5))

    , HU.testCase "✘ Early start = error on wrong type of jump penalty, a point penalty, applied, with other reset" $
        (FS.taskPoints
            (Early launchToStart1)
            (addSeq 0)
            nullSeqs{resets = [mkReset $ Just 0]}
            ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early launchToStart1, idSeq))

    , HU.testCase "✘ Early start = error on wrong type of penalty, a fraction penalty, applied, with other resete" $
        (FS.taskPoints
            (Early launchToStart1)
            (mulSeq 0.5)
            nullSeqs{resets = [mkReset $ Just 0]}
            ptsAllOne)
            @?=
                (Left $ WAT_Early_Jump (Early launchToStart1, mulSeq 0.5))

    , HU.testCase "✓ Early start tolerates no explicit jump penalty" $
        (FS.taskPoints (Early launchToStart1) idSeq nullSeqs ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 2
                    , total = TaskPoints 1
                    , effp = resetSeq $ Just 1
                    , effj = resetSeq $ Just 1
                    }

    , HU.testCase "✓ Early start tolerates other reset to zero" $
        (FS.taskPoints
            (Early launchToStart1)
            idSeq
            nullSeqs{resets = [mkReset $ Just 0]}
            ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 3
                    , total = TaskPoints 0
                    , effp = resetSeq $ Just 0
                    , effj = resetSeq $ Just 1
                    }

    , HU.testCase "✓ Early start still applies other penalty of fraction 1" $
        (FS.taskPoints (Early launchToStart1) idSeq nullSeqs{muls = [mkMul 1]} ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3
                    , mulApplied = TaskPoints 3
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0
                    , effp = resetSeq $ Just 1
                    , effj = resetSeq $ Just 1
                    }

    , HU.testCase "✘ Early start still applies other penalty of fraction 1" $
        (FS.taskPoints
            (Early launchToStart7)
            (resetSeq $ Just 11)
            nullSeqs{resets = [mkReset $ Just 1]}
            ptsAllOne)
            @?=
                (Left $ EQ_Early_Reset (Early launchToStart7, mkReset $ Just 11))
    ]
