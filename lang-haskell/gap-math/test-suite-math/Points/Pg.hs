module Points.Pg (pgUnits) where

import Data.Refined (assumeProp, refined)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified "flight-gap-math" Flight.Score as FS
import "flight-gap-weight" Flight.Score (EGwScaling(..))
import "flight-gap-math" Flight.Score
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
    , PenaltySeq(..)
    , PenaltySeqs(..)
    , zeroPoints
    , idSeq, nullSeqs
    , mulSeq, addSeq, resetSeq
    , mkMul, mkAdd, mkReset
    , egPenalty
    )

-- TODO: When base >= 4.11 use Data.Functor ((<&>))
(<&>) :: Either c a -> (a -> b) -> Either c b
(<&>) = flip (<$>)

egPgPenalty :: FS.GoalValidatedPoints -> FS.PenaltySeq
egPgPenalty = egPenalty $ EGwScaling 0

launchToStart0, launchToStart1, launchToStart2, launchToStart7 :: LaunchToStartPoints
launchToStart0 = LaunchToStartPoints . assumeProp $ refined 0
launchToStart1 = LaunchToStartPoints . assumeProp $ refined 1
launchToStart2 = LaunchToStartPoints . assumeProp $ refined 2
launchToStart7 = LaunchToStartPoints . assumeProp $ refined 7

ptsAllOne :: Points
ptsAllOne =
    Points
        { reach = LinearPoints 1
        , effort = DifficultyPoints 0
        , distance = DistancePoints 1
        , leading = LeadingPoints 1
        , arrival = ArrivalPoints 0
        , time = TimePoints 1
        }

essNoGoalPg :: PointsReduced
essNoGoalPg =
    PointsReduced
        { subtotal = TaskPoints 3
        , mulApplied = TaskPoints 0
        , addApplied = TaskPoints 1
        , resetApplied = TaskPoints 0
        , total = TaskPoints 2
        , effp = addSeq $ negate 1
        , effj = idSeq
        , effg = addSeq $ negate 1
        , rawj = nullSeqs
        }

pgUnits :: TestTree
pgUnits = testGroup "PG Points"
    [ testGroup "Nominal"
        [ HU.testCase "✓ Zero for each component sum to zero task points" $
            ((FS.taskPoints
                NominalPg
                egPgPenalty
                idSeq
                nullSeqs
                zeroPoints) <&> total)
                @?=
                    Right (TaskPoints 0)

        , HU.testCase "✓ Task points are the sum of reach, leading, time and arrival" $
            (FS.taskPoints
                NominalPg
                egPgPenalty
                idSeq
                nullSeqs
                ptsAllOne)
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✘ With jump penalty points" $
            (FS.taskPoints
                NominalPg
                egPgPenalty
                (addSeq $ negate 1)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_Nominal_Pg (NominalPg, addSeq $ negate 1, nullSeqs))

        , HU.testCase "✘ Applying jump penalty fraction" $
            (FS.taskPoints
                NominalPg
                egPgPenalty
                (mulSeq 0)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_Nominal_Pg (NominalPg, mulSeq 0, nullSeqs))

        , HU.testCase "✘ Applying jump penalty reset" $
            (FS.taskPoints
                NominalPg
                egPgPenalty
                (resetSeq $ Just 1)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_Nominal_Pg (NominalPg, resetSeq $ Just 1, nullSeqs))

    {- TODO: Failing test - memory leak?
        , HU.testCase "✓ Applying too many penalty points = zero" $
            (FS.taskPoints NominalPg idSeq nullSeqs{adds = [mkAdd $ negate 4]} ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3
                        , mulApplied = TaskPoints 0
                        , addApplied = TaskPoints 3
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 0
                        , effp = addSeq $ negate 4
                        , effj = idSeq
                        }
                        -}

    {- TODO: Failing test - memory leak?
        , HU.testCase "✓ Applying too many fraction points = zero" $
            (FS.taskPoints NominalPg idSeq nullSeqs{muls = [mkMul $ negate 2]} ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3
                        , mulApplied = TaskPoints 3
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 0
                        , effp = mulSeq 0
                        , effj = idSeq
                        }
                        -}

        , HU.testCase "✓ Applying a bonus fractio increases score" $
            (FS.taskPoints
                NominalPg
                egPgPenalty
                idSeq
                nullSeqs{muls = [mkMul 2]}
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3
                        , mulApplied = TaskPoints (-3)
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 6
                        , effp = mulSeq 2
                        , effj = idSeq
                        , effg = idSeq
                        , rawj = nullSeqs
                        }
        ]

    , testGroup "ESS but no goal"
        [ HU.testCase "✓ Task points are the sum of reach and leading, ignoring time and arrival" $
            (FS.taskPoints
                NoGoalPg
                egPgPenalty
                idSeq
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalPg

        , HU.testCase "✓ With jump penalty fraction == 1 (the identity of multiplication)" $
            (FS.taskPoints
                NoGoalPg
                egPgPenalty
                (mulSeq 1)
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalPg

        , HU.testCase "✓ With jump penalty points == 0 (the identity of addition)" $
            (FS.taskPoints
                NoGoalPg
                egPgPenalty
                (addSeq 0)
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalPg

        , HU.testCase "✓ XX With jump penalty points == ∅ (the identity of reset)" $
            (FS.taskPoints
                NoGoalPg
                (egPenalty $ EGwScaling 0)
                (addSeq 0)
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalPg
        ]

    , testGroup "Early"
        [ HU.testCase "✓ distance to start points only, ignoring effort and arrival, jump reset generated" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                idSeq
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ distance to start points only, ignoring effort and arrival, jump reset applied" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ other score-lowering reset can't raise the total" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (resetSeq $ Just 1)
                nullSeqs{adds = [mkAdd $ negate 1], muls = [mkMul 0.5]}
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3
                        , mulApplied = TaskPoints 1.5
                        , addApplied = TaskPoints 1
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 0.5
                        , effp =
                            idSeq
                                { mul = mkMul 0.5
                                , add = mkAdd $ negate 1
                                }
                        , effj = resetSeq $ Just 1
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ other non-reset score-raising penalties don't change total" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (resetSeq (Just 1))
                nullSeqs{adds = [mkAdd 1], muls = [mkMul 2]}
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3
                        , mulApplied = TaskPoints (-3)
                        , addApplied = TaskPoints (-1)
                        , resetApplied = TaskPoints 6
                        , total = TaskPoints 1
                        , effp =
                            idSeq
                                { mul = mkMul 2
                                , add = mkAdd 1
                                , reset = mkReset $ Just 1
                                }
                        , effj = resetSeq $ Just 1
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ taking the smallest reset penalty from jump and other sets" $
            (FS.taskPoints
                (Early launchToStart2)
                egPgPenalty
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✘ error on wrong reset applied" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (resetSeq $ Just 2)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ EQ_Early_Reset (Early launchToStart1, mkReset $ Just 2))

        , HU.testCase "✘ error on wrong type of jump penalty, a point penalty, applied" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (addSeq $ negate 1)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_Early_Jump (Early launchToStart1, addSeq $ negate 1))

        , HU.testCase "✘ error on wrong type of penalty, a fraction penalty, applied" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (mulSeq 0.5)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_Early_Jump (Early launchToStart1, mulSeq 0.5))

        , HU.testCase "✘ error on wrong type of penalty, a fraction penalty, applied, with other resets" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (mulSeq 0.5)
                nullSeqs{resets = [mkReset $ Just 0]}
                ptsAllOne)
                @?=
                    (Left $ WAT_Early_Jump (Early launchToStart1, mulSeq 0.5))

        , HU.testCase "✓ Tolerates no explicit jump penalty" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                idSeq
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ With jump penalty fraction = 1 (the identity of multiplication)" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (mulSeq 1)
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ With jump penalty points = 0 (the identity of addition)" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (addSeq 0)
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ With jump reset points = ∅ (the identity of reset)" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
                (resetSeq Nothing)
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ Overriden by other reset to zero" $
            (FS.taskPoints
                (Early launchToStart1)
                egPgPenalty
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
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

    {- TODO: Failing test - memory leak?
        , HU.testCase "✓ Overridden by other fraction 0" $
            (FS.taskPoints (Early launchToStart1) idSeq nullSeqs{muls = [mkMul 0]} ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 3
                        , mulApplied = TaskPoints 3
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 0
                        , effp = idSeq{mul = mkMul 0, reset = mkReset $ Just 1}
                        , effj = resetSeq $ Just 1
                        }
                        -}

        , HU.testCase "✘ Ignores other penalty fraction that would increase the score" $
            (FS.taskPoints
                (Early launchToStart7)
                egPgPenalty
                (resetSeq $ Just 11)
                nullSeqs{resets = [mkReset $ Just 1]}
                ptsAllOne)
                @?=
                    (Left $ EQ_Early_Reset (Early launchToStart7, mkReset $ Just 11))
        ]
    ]
