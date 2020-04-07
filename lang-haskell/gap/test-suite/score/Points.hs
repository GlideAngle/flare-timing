module Points (tallyUnits, taskPointsHg, taskPointsPg) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Function ((&))
import Data.Ratio ((%))
import Data.Maybe (listToMaybe)
import Data.Either (lefts, rights)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

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

tallyUnits :: TestTree
tallyUnits = testGroup "Tally task points, with and without penalties"
    [ HU.testCase "No penalties, no points = zero task points" $
        ((FS.taskPoints (Nothing :: Maybe (Penalty Hg)) [] [] zeroPoints) & total) @?= TaskPoints 0

    , HU.testCase "No penalties = sum of distance, leading, time & arrival points" $
        ((FS.taskPoints
            Nothing
            []
            []
            Points
                { reach = LinearPoints 1
                , effort = DifficultyPoints 1
                , distance = DistancePoints 2
                , leading = LeadingPoints 1
                , arrival = ArrivalPoints 1
                , time = TimePoints 1
                }) & total)
            @?= TaskPoints 5

    , HU.testCase "Early start PG = distance to start points only" $
        ((FS.taskPoints
            (Just (Early $ LaunchToStartPoints 1))
            []
            []
            Points
                { reach = LinearPoints 10
                , effort = DifficultyPoints 10
                , distance = DistancePoints 20
                , leading = LeadingPoints 10
                , arrival = ArrivalPoints 10
                , time = TimePoints 10
                }) & total)
            @?= TaskPoints 1

    , HU.testCase "Way too early start HG = minimum distance points only" $
        ((FS.taskPoints
            (Just (JumpedTooEarly $ TooEarlyPoints 1))
            []
            []
            Points
                { reach = LinearPoints 10
                , effort = DifficultyPoints 10
                , distance = DistancePoints 20
                , leading = LeadingPoints 10
                , arrival = ArrivalPoints 10
                , time = TimePoints 10
                }) & total)
            @?= TaskPoints 1

    , HU.testCase "Somewhat early start HG = full points minus jump the gun penalty" $
        let jump = JumpedTheGun [u| 1 s |]
            secs = SecondsPerPoint [u| 1 s |]
            limit = JumpTheGunLimit [u| 3000 s |]

            eitherPenalties :: [Either PointPenalty (Penalty Hg)]
            eitherPenalties =
                return $ FS.jumpTheGunPenaltyHg (TooEarlyPoints 1) limit secs jump

            jumpDemerits = lefts eitherPenalties
            jumpReset = listToMaybe $ rights eitherPenalties
        in
            (FS.taskPoints
                jumpReset
                jumpDemerits
                []
                Points
                    { reach = LinearPoints 10
                    , effort = DifficultyPoints 10
                    , distance = DistancePoints 20
                    , leading = LeadingPoints 10
                    , arrival = ArrivalPoints 10
                    , time = TimePoints 10
                    })
                @?= PointsReduced
                        { subtotal = TaskPoints 50
                        , fracApplied = TaskPoints 0
                        , pointApplied = TaskPoints 1
                        , resetApplied = TaskPoints 0
                        , total = TaskPoints 49
                        , effectivePenalties = [PenaltyPoints 1]
                        , effectivePenaltiesJump = [PenaltyPoints 1]
                        }
    ]

correct :: forall a. Maybe (Penalty a) -> Points -> TaskPoints -> Bool

correct
    Nothing
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    pts =
    pts == TaskPoints (r + e + l + t + a)

correct
    (Just (JumpedTooEarly (TooEarlyPoints md)))
    _
    pts =
    pts == TaskPoints (toRational md)

correct
    (Just (Jumped (SecondsPerPoint (MkQuantity spp)) (JumpedTheGun (MkQuantity jtg))))
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    pts =
    pts == TaskPoints (max 0 x)
    where
        x = (r + e + l + t + a) - (toRational $ jtg / spp)

correct
    (Just (JumpedNoGoal (SecondsPerPoint (MkQuantity spp)) (JumpedTheGun (MkQuantity jtg))))
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    pts =
    pts == TaskPoints (max 0 x)
    where
        x = (r + e + l + (8 % 10) * (t + a)) - (toRational $ jtg / spp)

correct
    (Just NoGoalHg)
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        }
    pts =
    pts == TaskPoints (r + e + l + (8 % 10) * (t + a))

correct (Just (Early (LaunchToStartPoints lts))) Points{..} (TaskPoints pts) =
    pts == toRational lts

correct
    (Just NoGoalPg)
    Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        }
    pts =
    pts == TaskPoints (r + e + l)

taskPointsHg :: PtTest Hg -> Bool
taskPointsHg (PtTest (penalty, parts)) =
    correct penalty parts $ (FS.taskPoints penalty [] [] parts) & total

taskPointsPg :: PtTest Pg -> Bool
taskPointsPg (PtTest (penalty, parts)) =
    correct penalty parts $ (FS.taskPoints penalty [] [] parts) & total
