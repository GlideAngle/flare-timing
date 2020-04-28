module Points.Hg (hgUnits) where

import Data.Refined (assumeProp, refined)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Either (lefts, rights)
import Data.UnitsOfMeasure (u)

import qualified Flight.Score as FS
import Flight.Score
    ( TooEarlyPoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , JumpTheGunLimit(..)
    , Hg
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
    , PosInt
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

posint1 :: PosInt
posint1 = assumeProp $ refined 1

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
        (FS.taskPoints (JumpedTooEarly (TooEarlyPoints $ assumeProp (refined 1))) [] [] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 5
                    , fracApplied = TaskPoints 0
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 4
                    , total = TaskPoints 1
                    , effectivePenalties = [PenaltyReset posint1]
                    , effectivePenaltiesJump = [PenaltyReset posint1]
                    }

    , HU.testCase "Way too early start with fraction other > 1" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints posint1) [PenaltyReset posint1] [PenaltyFraction 2] ptsAllOne)
            @?=
                Right
                PointsReduced
                    { subtotal = TaskPoints 5
                    , fracApplied = TaskPoints 5
                    , pointApplied = TaskPoints 0
                    , resetApplied = TaskPoints 0
                    , total = TaskPoints 0
                    , effectivePenalties = [PenaltyFraction 2, PenaltyReset posint1]
                    , effectivePenaltiesJump = [PenaltyReset posint1]
                    }

    , HU.testCase "Early start, ESS but no goal = ESS no goal points minus jump the gun penalty" $
        let jump = JumpedTheGun [u| 2 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints posint1) limit secs jump in [x]
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

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints posint1) limit secs jump in [x]
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

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints posint1) limit secs jump in [x]
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

            jumps = let Left x = FS.jumpTheGunSitRepHg (TooEarlyPoints posint1) limit secs jump in [x]
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
                return $ FS.jumpTheGunSitRepHg (TooEarlyPoints posint1) limit secs jump

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
                        , effectivePenalties = [PenaltyReset posint1]
                        , effectivePenaltiesJump = [PenaltyReset posint1]
                        }

    , HU.testCase "Early start = error on wrong reset applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints posint1) [PenaltyReset (assumeProp $ refined 2)] [] ptsAllOne)
            @?=
                (Left $ EQ_JumpedTooEarly_Reset (JumpedTooEarly (TooEarlyPoints posint1),[PenaltyReset (assumeProp $ refined 2)]))

    , HU.testCase "Early start = error on wrong type of penalty, a point penalty, applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints posint1) [PenaltyPoints 1] [] ptsAllOne)
            @?=
                (Left $ WAT_JumpedTooEarly (JumpedTooEarly (TooEarlyPoints posint1),[PenaltyPoints 1.0],[]))

    , HU.testCase "Early start = error on wrong type of penalty, a fraction penalty, applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints posint1) [PenaltyFraction 0.5] [] ptsAllOne)
            @?=
                (Left $ WAT_JumpedTooEarly (JumpedTooEarly (TooEarlyPoints posint1),[PenaltyFraction 0.5],[]))
    ]
