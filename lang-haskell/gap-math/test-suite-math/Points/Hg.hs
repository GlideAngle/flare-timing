module Points.Hg (hgUnits) where

import Data.Refined (assumeProp, refined)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u)

import qualified "flight-gap-math" Flight.Score as FS
import "flight-gap-weight" Flight.Score (EGwScaling(..))
import "flight-gap-math" Flight.Score
    ( TooEarlyPoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , JumpTheGunLimit(..)
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
    , idSeq, nullSeqs, toSeqs
    , mulSeq, addSeq, resetSeq
    , mkReset
    , exAdd, mkAdd
    , egPenalty
    -- NOTE: imports needed for memory leaking, stack overflowing test.
    --, PenaltySeq(..), mkMull
    )
import Points.Round ((<&>), dpRoundPointsReduced)

egHgPenalty :: FS.GoalValidatedPoints -> FS.PenaltySeq
egHgPenalty = egPenalty $ EGwScaling 0.8

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

tooEarly0, tooEarly1, tooEarly2 :: TooEarlyPoints
tooEarly0 = TooEarlyPoints . assumeProp $ refined 0
tooEarly1 = TooEarlyPoints . assumeProp $ refined 1
tooEarly2 = TooEarlyPoints . assumeProp $ refined 2

essNoGoalHg :: PointsReduced
essNoGoalHg =
    PointsReduced
        { subtotal = TaskPoints 5
        , mulApplied = TaskPoints 0
        , addApplied = TaskPoints 0.4
        , resetApplied = TaskPoints 0
        , total = TaskPoints 4.6
        , effp = addSeq $ negate 0.4
        , effj = idSeq
        , effg = addSeq $ negate 0.4
        , rawj = nullSeqs
        }

essNoGoalEarlyHg :: PointsReduced
essNoGoalEarlyHg =
    essNoGoalHg
        { addApplied = TaskPoints 2
        , total = TaskPoints 3
        , effp = addSeq $ negate 2
        , effj = addSeq $ negate 2
        , effg = idSeq
        , rawj = toSeqs . addSeq $ negate 2
        }

hgUnits :: TestTree
hgUnits = testGroup "HG Points"
    [ testGroup "Nominal"
        [ HU.testCase "✓ Zero for each component sum to zero task points" $
            ((FS.taskPoints
                NominalHg
                egHgPenalty
                idSeq
                nullSeqs
                zeroPoints)
                <&> total)
                @?=
                    Right (TaskPoints 0)

        , HU.testCase "✓ Task points are the sum of reach, effort, leading, time & arrival points" $
            (FS.taskPoints
                NominalHg
                egHgPenalty
                idSeq
                nullSeqs
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , mulApplied = TaskPoints 0
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 5
                        , effp = idSeq
                        , effj = idSeq
                        , effg = idSeq
                        , rawj = nullSeqs
                        }
        ]

    , testGroup "Early start"
        [ HU.testCase "✓ Full points minus jump the gun penalty" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 3 s |]

                Left j = FS.jumpTheGunSitRepHg tooEarly1 limit secs jump
            in
                (FS.taskPoints
                    (Jumped tooEarly0 secs jump)
                    egHgPenalty
                    (addSeq $ exAdd j)
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right
                        PointsReduced
                            { subtotal = TaskPoints 5
                            , mulApplied = TaskPoints 0
                            , addApplied = TaskPoints 2
                            , resetApplied = TaskPoints 0
                            , total = TaskPoints 3
                            , effp = addSeq $ negate 2
                            , effj = addSeq $ negate 2
                            , effg = idSeq
                            , rawj = toSeqs . addSeq $ negate 2
                            }

        , HU.testCase "✓ Very early start = full points minus jump the gun penalty" $
            let jump = JumpedTheGun [u| 3 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 3 s |]

                Left j = FS.jumpTheGunSitRepHg tooEarly1 limit secs jump
            in
                (FS.taskPoints
                    (Jumped tooEarly0 secs jump)
                    egHgPenalty
                    (addSeq $ exAdd j)
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right
                        PointsReduced
                            { subtotal = TaskPoints 5
                            , mulApplied = TaskPoints 0
                            , addApplied = TaskPoints 3
                            , resetApplied = TaskPoints 0
                            , total = TaskPoints 2
                            , effp = addSeq $ negate 3
                            , effj = addSeq $ negate 3
                            , effg = idSeq
                            , rawj = toSeqs . addSeq $ negate 3
                            }

        , HU.testCase "✓ Early start = no less than zero" $
            let jump = JumpedTheGun [u| 10 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 11 s |]

                Left j = FS.jumpTheGunSitRepHg tooEarly1 limit secs jump
            in
                (FS.taskPoints
                    (Jumped tooEarly0 secs jump)
                    egHgPenalty
                    (addSeq $ exAdd j)
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right
                        PointsReduced
                            { subtotal = TaskPoints 5
                            , mulApplied = TaskPoints 0
                            , addApplied = TaskPoints 5
                            , resetApplied = TaskPoints 0
                            , total = TaskPoints 0
                            , effp = addSeq $ negate 5
                            , effj = addSeq $ negate 5
                            , effg = idSeq
                            , rawj = nullSeqs {adds = [mkAdd $ negate 10, mkAdd 5]}
                            }
        ]

    , testGroup "Too early start"
        [ HU.testCase "✓ Task points of minimum distance points only" $
            (FS.taskPoints
                (JumpedTooEarly tooEarly1)
                egHgPenalty
                idSeq
                nullSeqs
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , mulApplied = TaskPoints 0
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 4
                        , total = TaskPoints 1
                        , effp = resetSeq $ Just 1
                        , effj = resetSeq $ Just 1
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

    {- TODO: Failing test - memory leak?
        , HU.testCase "✓ With other penalty, fraction <= 0 => zero task points" $
            (FS.taskPoints
                (JumpedTooEarly tooEarly1)
                (resetSeq $ Just 1)
                nullSeqs{muls = [mkMul 0]}
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , mulApplied = TaskPoints 5
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 0
                        , effp = idSeq{mul = mkMul 0, reset = mkReset $ Just 1}
                        , effj = resetSeq $ Just 1
                        }
                        -}

        , HU.testCase "✓ With smaller other penalty, a reset" $
            (FS.taskPoints
                (JumpedTooEarly tooEarly2)
                egHgPenalty
                (resetSeq $ Just 2)
                nullSeqs{resets = [mkReset $ Just 1]}
                ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , mulApplied = TaskPoints 0
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 4
                        , total = TaskPoints 1
                        , effp = resetSeq $ Just 1
                        , effj = resetSeq $ Just 2
                        , effg = idSeq
                        , rawj = nullSeqs
                        }

        , HU.testCase "✓ Points for minimum distance" $
            let jump = JumpedTheGun [u| 4 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 3 s |]

                (sitrep, js) =
                    case FS.jumpTheGunSitRepHg tooEarly1 limit secs jump of
                        Left j -> (Jumped tooEarly1 secs jump, addSeq . exAdd $ negate j)
                        Right p@(JumpedTooEarly _) -> (p, resetSeq (Just 1))
                        Right _ -> error "Unexpected jump the gun situation."
            in
                (FS.taskPoints
                    sitrep
                    egHgPenalty
                    js
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right
                        PointsReduced
                            { subtotal = TaskPoints 5
                            , mulApplied = TaskPoints 0
                            , addApplied = TaskPoints 0
                            , resetApplied = TaskPoints 4
                            , total = TaskPoints 1
                            , effp = resetSeq $ Just 1
                            , effj = resetSeq $ Just 1
                            , effg = idSeq
                            , rawj = nullSeqs
                            }

        , HU.testCase "✘ Error on wrong reset applied" $
            (FS.taskPoints
                (JumpedTooEarly tooEarly1)
                egHgPenalty
                (resetSeq (Just 2))
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ EQ_JumpedTooEarly_Reset (JumpedTooEarly tooEarly1, mkReset (Just 2)))

        , HU.testCase "✘ Error on wrong type of penalty, a point penalty, applied" $
            (FS.taskPoints
                (JumpedTooEarly tooEarly1)
                egHgPenalty
                (addSeq $ negate 1)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_JumpedTooEarly (JumpedTooEarly tooEarly1, addSeq $ negate 1, nullSeqs))

        , HU.testCase "✘ Error on wrong type of penalty, a fraction penalty, applied" $
            (FS.taskPoints (JumpedTooEarly tooEarly1) egHgPenalty (mulSeq 0.5) nullSeqs ptsAllOne)
                @?=
                    (Left $ WAT_JumpedTooEarly (JumpedTooEarly tooEarly1, mulSeq 0.5, nullSeqs))
        ]

    , testGroup "ESS but no goal"
        [ HU.testCase "✓ Sum of reach, effort, leading, 80% of time & arrival points" $
            (dpRoundPointsReduced <$> FS.taskPoints
                NoGoalHg
                egHgPenalty
                idSeq
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✓ With jump penalty fraction = 1 (the identity of multiplication)" $
            (dpRoundPointsReduced <$> FS.taskPoints
                NoGoalHg
                egHgPenalty
                (mulSeq 1)
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✘ With jump penalty points = 0 (the identity of addition)" $
            (dpRoundPointsReduced <$> FS.taskPoints
                NoGoalHg
                egHgPenalty
                (addSeq 0)
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✘ With jump reset = ∅ (the identity of reset)" $
            (dpRoundPointsReduced <$> FS.taskPoints
                NoGoalHg
                egHgPenalty
                (addSeq 0)
                nullSeqs
                ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✘ With jump penalty fraction = 0 (scale to zero)" $
            (FS.taskPoints
                NoGoalHg
                egHgPenalty
                (mulSeq 0)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_NoGoal_Hg (NoGoalHg, mulSeq 0))

        , HU.testCase "✘ With other penalty fraction = 0 (scale to zero)" $
            (FS.taskPoints
                NoGoalHg
                egHgPenalty
                (mulSeq 0)
                nullSeqs
                ptsAllOne)
                @?=
                    (Left $ WAT_NoGoal_Hg (NoGoalHg, mulSeq 0))
        ]

    , testGroup "ESS but no goal *and* Early start"
        [ HU.testCase "✓ ESS sum minus jump the gun penalty" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 3 s |]

                Left j = FS.jumpTheGunSitRepHg tooEarly1 limit secs jump
            in
                (FS.taskPoints
                    (JumpedNoGoal tooEarly1 secs jump)
                    egHgPenalty
                    (addSeq $ exAdd j)
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right essNoGoalEarlyHg

        , HU.testCase "✘ With jump penalty fraction = 1 (the identity of multiplication)" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
            in
                (FS.taskPoints
                    (JumpedNoGoal tooEarly1 secs jump)
                    egHgPenalty
                    (mulSeq 1)
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right essNoGoalEarlyHg

        , HU.testCase "✘ With jump penalty points = 0 (the identity of addition)" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
            in
                (FS.taskPoints
                    (JumpedNoGoal tooEarly1 secs jump)
                    egHgPenalty
                    (addSeq 0)
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right essNoGoalEarlyHg

        , HU.testCase "✘ With jump penalty reset = ∅ (the identity of reset)" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
            in
                (FS.taskPoints
                    (JumpedNoGoal tooEarly1 secs jump)
                    egHgPenalty
                    (resetSeq Nothing)
                    nullSeqs
                    ptsAllOne)
                    @?=
                        Right essNoGoalEarlyHg
        ]
    ]
