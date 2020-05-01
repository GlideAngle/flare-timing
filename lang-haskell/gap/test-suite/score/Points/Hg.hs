module Points.Hg (hgUnits) where

import Data.Refined (assumeProp, refined)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u)

import qualified Flight.Score as FS
import Flight.Score
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
    , idSeq, nullSeqs
    , mulSeq, addSeq, resetSeq
    , mkMul, mkReset
    , exAdd
    , identityOfAdd
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

tooEarly1, tooEarly2 :: TooEarlyPoints
tooEarly1 = TooEarlyPoints . assumeProp $ refined 1
tooEarly2 = TooEarlyPoints . assumeProp $ refined 2

essNoGoalHg :: PointsReduced
essNoGoalHg =
    PointsReduced
        { subtotal = TaskPoints 3.6
        , mulApplied = TaskPoints 0
        , addApplied = TaskPoints 0
        , resetApplied = TaskPoints 0
        , total = TaskPoints 3.6
        , effp = idSeq
        , effj = idSeq
        }

hgUnits :: TestTree
hgUnits = testGroup "HG Points"
    [ testGroup "No penalties"
        [ HU.testCase "✓ Zero for each component sum to zero task points" $
            ((FS.taskPoints NominalHg idSeq nullSeqs zeroPoints) <&> total) @?= Right (TaskPoints 0)

        , HU.testCase "✓ Task points are the sum of reach, effort, leading, time & arrival points" $
            (FS.taskPoints NominalHg idSeq nullSeqs ptsAllOne)
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
                        }
        ]

    , testGroup "Early start"
        [ HU.testCase "✓ Full points minus jump the gun penalty" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 3 s |]

                Left j = FS.jumpTheGunSitRepHg tooEarly1 limit secs jump
            in
                (FS.taskPoints (Jumped secs jump) (addSeq $ exAdd j) nullSeqs ptsAllOne)
                    @?=
                        Right
                        PointsReduced
                            { subtotal = TaskPoints 5
                            , mulApplied = TaskPoints 0
                            , addApplied = TaskPoints 2
                            , resetApplied = TaskPoints 0
                            , total = TaskPoints 3
                            , effp = addSeq 2
                            , effj = addSeq 2
                            }

        , HU.testCase "✓ Very early start = full points minus jump the gun penalty resulting in no less than zero" $
            let jump = JumpedTheGun [u| 3 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 3 s |]

                Left j = FS.jumpTheGunSitRepHg tooEarly1 limit secs jump
            in
                (FS.taskPoints (Jumped secs jump) (addSeq $ exAdd j) nullSeqs ptsAllOne)
                    @?=
                        Right
                        PointsReduced
                            { subtotal = TaskPoints 5
                            , mulApplied = TaskPoints 0
                            , addApplied = TaskPoints 3
                            , resetApplied = TaskPoints 0
                            , total = TaskPoints 2
                            , effp = addSeq 3
                            , effj = addSeq 3
                            }
        ]

    , testGroup "Too early start"
        [ HU.testCase "✓ Task points of minimum distance points only" $
            (FS.taskPoints (JumpedTooEarly tooEarly1) idSeq nullSeqs ptsAllOne)
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
                        }

        , HU.testCase "✓ With fraction other > 1 zeroes out task points" $
            (FS.taskPoints (JumpedTooEarly tooEarly1) (resetSeq $ Just 1) nullSeqs{muls = [mkMul 2]} ptsAllOne)
                @?=
                    Right
                    PointsReduced
                        { subtotal = TaskPoints 5
                        , mulApplied = TaskPoints 5
                        , addApplied = TaskPoints 0
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 0
                        , effp = mulSeq 2
                        , effj = resetSeq $ Just 1
                        }

        , HU.testCase "✓ With smaller other penalty, a reset" $
            (FS.taskPoints
                (JumpedTooEarly tooEarly2)
                (resetSeq (Just 2))
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
                        , effp = resetSeq $ Just 2
                        , effj = resetSeq $ Just 2
                        }

        , HU.testCase "✓ Points for minimum distance" $
            let jump = JumpedTheGun [u| 4 s |]
                secs = SecondsPerPoint [u| 1 s |]
                limit = JumpTheGunLimit [u| 3 s |]

                (sitrep, js) =
                    case FS.jumpTheGunSitRepHg tooEarly1 limit secs jump of
                        Left j -> (Jumped secs jump, addSeq $ exAdd j)
                        Right p@(JumpedTooEarly _) -> (p, resetSeq (Just 1))
                        Right _ -> error "Unexpected jump the gun situation."
            in
                (FS.taskPoints
                    sitrep
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
                            }

        , HU.testCase "✘ Error on wrong reset applied" $
            (FS.taskPoints (JumpedTooEarly tooEarly1) (resetSeq (Just 2)) nullSeqs ptsAllOne)
                @?=
                    (Left $ EQ_JumpedTooEarly_Reset (JumpedTooEarly tooEarly1, mkReset (Just 2)))

        , HU.testCase "✘ Error on wrong type of penalty, a point penalty, applied" $
            (FS.taskPoints (JumpedTooEarly tooEarly1) (addSeq 1) nullSeqs ptsAllOne)
                @?=
                    (Left $ WAT_JumpedTooEarly (JumpedTooEarly tooEarly1, addSeq 1.0, nullSeqs))

        , HU.testCase "✘ Error on wrong type of penalty, a fraction penalty, applied" $
            (FS.taskPoints (JumpedTooEarly tooEarly1) (mulSeq 0.5) nullSeqs ptsAllOne)
                @?=
                    (Left $ WAT_JumpedTooEarly (JumpedTooEarly tooEarly1, mulSeq 0.5, nullSeqs))
        ]
    , testGroup "ESS but no goal"
        [ HU.testCase "✓ Sum of reach, effort, leading, 80% of time & arrival points" $
            (FS.taskPoints NoGoalHg idSeq nullSeqs ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✓ With jump penalty fraction = 1 (the identity of multiplication)" $
            (FS.taskPoints NoGoalHg (mulSeq 1) nullSeqs ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✘ With jump penalty points = 0 (the identity of addition)" $
            (FS.taskPoints NoGoalHg (addSeq 0) nullSeqs ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✘ With jump reset = ∅ (the identity of reset)" $
            (FS.taskPoints NoGoalHg (addSeq 0) nullSeqs ptsAllOne)
                @?=
                    Right essNoGoalHg

        , HU.testCase "✘ With jump penalty fraction = 0 (scale to zero)" $
            (FS.taskPoints NoGoalHg (mulSeq 0) nullSeqs ptsAllOne)
                @?=
                    (Left $ WAT_NoGoal_Hg (NoGoalHg, mulSeq 0))

        , HU.testCase "✘ With other penalty fraction = 0 (scale to zero)" $
            (FS.taskPoints NoGoalHg (mulSeq 0) nullSeqs ptsAllOne)
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
                (FS.taskPoints (JumpedNoGoal secs jump) (addSeq $ exAdd j) nullSeqs ptsAllOne)
                    @?=
                        Right
                        PointsReduced
                            { subtotal = TaskPoints 3.6
                            , mulApplied = TaskPoints 0
                            , addApplied = TaskPoints 2
                            , resetApplied = TaskPoints 0
                            , total = TaskPoints 1.6
                            , effp = addSeq 2
                            , effj = addSeq 2
                            }

        , HU.testCase "✘ With jump penalty fraction = 1 (the identity of multiplication)" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
            in
                (FS.taskPoints (JumpedNoGoal secs jump) (addSeq 0) nullSeqs ptsAllOne)
                    @?=
                        Right essNoGoalHg

        , HU.testCase "✘ With jump penalty points = 0 (the identity of addition)" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
            in
                (FS.taskPoints (JumpedNoGoal secs jump) (addSeq 0) nullSeqs ptsAllOne)
                    @?=
                        Right essNoGoalHg

        , HU.testCase "✘ With jump penalty reset = ∅ (the identity of reset)" $
            let jump = JumpedTheGun [u| 2 s |]
                secs = SecondsPerPoint [u| 1 s |]
            in
                (FS.taskPoints (JumpedNoGoal secs jump) (addSeq 0) nullSeqs ptsAllOne)
                    @?=
                        Right essNoGoalHg
        ]
    ]
