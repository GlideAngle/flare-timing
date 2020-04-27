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
    , Penalty(..)
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
    , zeroPoints
    )

import TestNewtypes

-- TODO: When base >= 4.11 use Data.Functor ((<&>))
(<&>) :: Either String a -> (a -> b) -> Either String b
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
        ((FS.taskPoints NoPenaltyHg [] [] zeroPoints) <&> total) @?= Right (TaskPoints 0)

    , HU.testCase "No penalties = sum of reach, effort, leading, time & arrival points" $
        (FS.taskPoints NoPenaltyHg [] [] ptsAllOne)
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

    , HU.testCase "Early start = full points minus jump the gun penalty" $
        let jump = JumpedTheGun [u| 2 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            jumps = let Left x = FS.jumpTheGunPenaltyHg (TooEarlyPoints 1) limit secs jump in [x]
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

    , HU.testCase "Very early start = full points minus jump the gun penalty resulting in no less than zero" $
        let jump = JumpedTheGun [u| 3 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3 s |]

            jumps = let Left x = FS.jumpTheGunPenaltyHg (TooEarlyPoints 1) limit secs jump in [x]
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

            eitherPenalties :: [Either PointPenalty (Penalty Hg)]
            eitherPenalties =
                return $ FS.jumpTheGunPenaltyHg (TooEarlyPoints 1) limit secs jump

            jumpDemerits = lefts eitherPenalties
            jumpReset = fromMaybe NoPenaltyHg . listToMaybe $ rights eitherPenalties
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
                Left "Early HG with launch to start points /= reset but got (JumpedTooEarly (TooEarlyPoints 1),[PenaltyReset 2])"

    , HU.testCase "Early start = error on wrong type of penalty, a point penalty, applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints 1) [PenaltyPoints 1] [] ptsAllOne)
            @?=
                Left "Early HG with unexpected reconciliation of (JumpedTooEarly (TooEarlyPoints 1),[PenaltyPoints 1.0],[])"

    , HU.testCase "Early start = error on wrong type of penalty, a fraction penalty, applied" $
        (FS.taskPoints (JumpedTooEarly $ TooEarlyPoints 1) [PenaltyFraction 0.5] [] ptsAllOne)
            @?=
                Left "Early HG with unexpected reconciliation of (JumpedTooEarly (TooEarlyPoints 1),[PenaltyFraction 0.5],[])"
    ]

pgUnits :: TestTree
pgUnits = testGroup "PG Points"
    [ HU.testCase "No penalties and no points = zero task points" $
        ((FS.taskPoints NoPenaltyPg [] [] zeroPoints) <&> total) @?= Right (TaskPoints 0)

    , HU.testCase "No penalties = sum of reach, leading, time points, ignoring effort and arrival" $
        (FS.taskPoints NoPenaltyPg [] [] ptsAllOne)
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

    , HU.testCase "Early start = ignoring other non-reset penalties" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyReset 1] [PenaltyPoints 1, PenaltyFraction 0.5] ptsAllOne)
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
                Left "Early PG with launch to start points /= reset but got (Early (LaunchToStartPoints 1),[PenaltyReset 2])"

    , HU.testCase "Early start = error on wrong type of penalty, a point penalty, applied" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyPoints 1] [] ptsAllOne)
            @?=
                Left "Early PG with unexpected jump penalties (Early (LaunchToStartPoints 1),[PenaltyPoints 1.0])"

    , HU.testCase "Early start = error on wrong type of penalty, a fraction penalty, applied" $
        (FS.taskPoints (Early $ LaunchToStartPoints 1) [PenaltyFraction 0.5] [] ptsAllOne)
            @?=
                Left "Early PG with unexpected jump penalties (Early (LaunchToStartPoints 1),[PenaltyFraction 0.5])"
    ]

correct
    :: forall a. (Penalty a)
    -> [PointPenalty]
    -> [PointPenalty]
    -> Points
    -> TaskPoints

correct
    NoPenaltyHg
    jumps
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = p
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        p = FS.applyPenalties (jumps ++ others) x

correct
    NoGoalHg
    jumps
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = p
    where
        x = TaskPoints (fromRational $ r + e + l + ((8 % 10) * (a + t)))
        p = FS.applyPenalties (jumps ++ others) x

correct
    JumpedTooEarly{}
    jumps
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = p
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        p = FS.applyPenalties (jumps ++ others) x

correct
    Jumped{}
    jumps
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = p
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        p = FS.applyPenalties (jumps ++ others) x

correct
    JumpedNoGoal{}
    jumps
    others
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    = p
    where
        x = TaskPoints (fromRational $ r + e + l + t + a)
        p = FS.applyPenalties (jumps ++ others) x

correct
    NoPenaltyPg
    jumps
    others
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        , time = TimePoints t
        }
    = p
    where
        x = TaskPoints (fromRational $ r + l + t)
        p = FS.applyPenalties (jumps ++ others) x

correct
    NoGoalPg
    jumps
    others
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        }
    = p
    where
        x = TaskPoints (fromRational $ r + l)
        p = FS.applyPenalties (jumps ++ others) x

correct
    Early{}
    jumps
    others
    Points
        { reach = LinearPoints r
        , leading = LeadingPoints l
        , time = TimePoints t
        }
    = p
    where
        x = TaskPoints (fromRational $ r + l + t)
        p = FS.applyPenalties (jumps ++ others) x

hgTaskPoints :: PtTest Hg -> Bool
hgTaskPoints (PtTest (penalty, jumps, others, parts)) =
    let expected = correct penalty jumps others parts
        actual = (FS.taskPoints penalty jumps others parts) <&> total
    in actual == Right expected

pgTaskPoints :: PtTest Pg -> Bool
pgTaskPoints (PtTest (penalty, jumps, others, parts)) =
    let expected = correct penalty jumps others parts
        actual = (FS.taskPoints penalty jumps others parts) <&> total
    in actual == Right expected
