module LeadingCoefficient (leadingCoefficientUnits) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Score as FS
import Flight.Score
    ( TaskTime(..)
    , DistanceToEss(..)
    , LcTrack(..)
    )

leadingCoefficientUnits :: TestTree
leadingCoefficientUnits = testGroup "Leading coefficient unit tests"
    [ madeGoalUnits
    ]

madeGoalUnits :: TestTree
madeGoalUnits = testGroup "Made goal unit tests"
    [ HU.testCase "Single point, at goal = made goal" $
        FS.madeGoal (LcTrack [ (TaskTime 0, DistanceToEss 0) ]) @?= True

    , HU.testCase "Single point, not at goal = didn't make goal" $
        FS.madeGoal (LcTrack [ (TaskTime 0, DistanceToEss 1) ]) @?= False

    , HU.testCase "Two points, not at goal = didn't make goal" $
        FS.madeGoal (LcTrack [ (TaskTime 0, DistanceToEss 2)
                             , (TaskTime 1, DistanceToEss 1)
                             ]) @?= False

    , HU.testCase "Two points, only last point at goal = made goal" $
        FS.madeGoal (LcTrack [ (TaskTime 0, DistanceToEss 1)
                             , (TaskTime 1, DistanceToEss 0)
                             ]) @?= True

    , HU.testCase "Two points, only first point at goal = made goal" $
        FS.madeGoal (LcTrack [ (TaskTime 0, DistanceToEss 0)
                             , (TaskTime 1, DistanceToEss 1)
                             ]) @?= True
    ]
