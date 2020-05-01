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

hgUnits :: TestTree
hgUnits = testGroup "HG Points"
    [ HU.testCase "No penalties and no points = zero task points" $
        ((FS.taskPoints NominalHg idSeq nullSeqs zeroPoints) <&> total) @?= Right (TaskPoints 0)

    , HU.testCase "No penalties = sum of reach, effort, leading, time & arrival points" $
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

    , HU.testCase "No penalties, ESS but no goal = sum of reach, effort, leading, 80% of time & arrival points" $
        (FS.taskPoints NoGoalHg idSeq nullSeqs ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 3.6
                    , mulApplied = TaskPoints 0
                    , addApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 3.6
                    , effp = idSeq
                    , effj = idSeq
                    }

    , HU.testCase "No penalties, ESS but no goal with jump penalty points" $
        (FS.taskPoints NoGoalHg (addSeq 0) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Hg (NoGoalHg, idSeq))

    , HU.testCase "No penalties, ESS but no goal with jump penalty fraction" $
        (FS.taskPoints NoGoalHg (mulSeq 0) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_NoGoal_Hg (NoGoalHg, idSeq))

    , HU.testCase "Way too early start = minimum distance points only" $
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

    , HU.testCase "Way too early start with fraction other > 1" $
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

    , HU.testCase "Way too early start with smaller reset other" $
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

    , HU.testCase "Early start, ESS but no goal = ESS no goal points minus jump the gun penalty" $
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

    , HU.testCase "Early start, ESS but no goal with zero jump penalty" $
        let jump = JumpedTheGun [u| 2 s |]
            secs = SecondsPerPoint [u| 1 s |]
        in
            (FS.taskPoints (JumpedNoGoal secs jump) (addSeq 0) nullSeqs ptsAllOne)
                @?=
                    (Left $ EQ_Jumped_Point (JumpedNoGoal (SecondsPerPoint [u| 1.0 s |]) (JumpedTheGun [u| 2.0 s |]), identityOfAdd))

    , HU.testCase "Early start = full points minus jump the gun penalty" $
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

    , HU.testCase "Early start and no goal = full points minus jump the gun penalty" $
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

    , HU.testCase "Very early start = full points minus jump the gun penalty resulting in no less than zero" $
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

    , HU.testCase "Too early start = points for minimum distance" $
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

    , HU.testCase "Early start = error on wrong reset applied" $
        (FS.taskPoints (JumpedTooEarly tooEarly1) (resetSeq (Just 2)) nullSeqs ptsAllOne)
            @?=
                (Left $ EQ_JumpedTooEarly_Reset (JumpedTooEarly tooEarly1, mkReset (Just 2)))

    , HU.testCase "Early start = error on wrong type of penalty, a point penalty, applied" $
        (FS.taskPoints (JumpedTooEarly tooEarly1) (addSeq 1) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_JumpedTooEarly (JumpedTooEarly tooEarly1, addSeq 1.0, nullSeqs))

    , HU.testCase "Early start = error on wrong type of penalty, a fraction penalty, applied" $
        (FS.taskPoints (JumpedTooEarly tooEarly1) (mulSeq 0.5) nullSeqs ptsAllOne)
            @?=
                (Left $ WAT_JumpedTooEarly (JumpedTooEarly tooEarly1, mulSeq 0.5, nullSeqs))
    ]
