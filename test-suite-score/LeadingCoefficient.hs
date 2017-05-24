module LeadingCoefficient
    ( leadingCoefficientUnits
    , cleanTrack
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Score as FS
import Flight.Score
    ( TaskTime(..)
    , DistanceToEss(..)
    , LcTrack(..)
    )

import TestNewtypes

leadingCoefficientUnits :: TestTree
leadingCoefficientUnits = testGroup "Leading coefficient unit tests"
    [ madeGoalUnits
    , cleanTrackUnits
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

cleanTrackUnits :: TestTree
cleanTrackUnits = testGroup "Clean track unit tests"
    [ HU.testCase "Single point = no points removed" $
        FS.cleanTrack (LcTrack [ (TaskTime 0, DistanceToEss 0) ])
        @?= LcTrack [ (TaskTime 0, DistanceToEss 0) ]

    , HU.testCase "Two points, each one closer to goal = no points removed" $
        FS.cleanTrack (LcTrack [ (TaskTime 0, DistanceToEss 2)
                               , (TaskTime 1, DistanceToEss 1)
                               ])
        @?= LcTrack [ (TaskTime 0, DistanceToEss 2) 
                    , (TaskTime 1, DistanceToEss 1)
                    ]

    , HU.testCase "Two points, each one further from goal = all but one point removed" $
        FS.cleanTrack (LcTrack [ (TaskTime 0, DistanceToEss 1)
                               , (TaskTime 1, DistanceToEss 2)
                               ])
        @?= LcTrack [ (TaskTime 0, DistanceToEss 1) ]

    , HU.testCase "Three points, each one closer to goal = no points removed" $
        FS.cleanTrack (LcTrack [ (TaskTime 0, DistanceToEss 3)
                               , (TaskTime 1, DistanceToEss 2)
                               , (TaskTime 2, DistanceToEss 1)
                               ])
        @?= LcTrack [ (TaskTime 0, DistanceToEss 3) 
                    , (TaskTime 1, DistanceToEss 2)
                    , (TaskTime 2, DistanceToEss 1)
                    ]

    , HU.testCase "Three points, each one further from goal = all but one point removed" $
        FS.cleanTrack (LcTrack [ (TaskTime 0, DistanceToEss 1)
                               , (TaskTime 1, DistanceToEss 2)
                               , (TaskTime 1, DistanceToEss 3)
                               ])
        @?= LcTrack [ (TaskTime 0, DistanceToEss 1) ]

    , HU.testCase "Three points, only 2nd moves further from goal = that point removed" $
        FS.cleanTrack (LcTrack [ (TaskTime 0, DistanceToEss 2)
                               , (TaskTime 1, DistanceToEss 3)
                               , (TaskTime 2, DistanceToEss 1)
                               ])
        @?= LcTrack [ (TaskTime 0, DistanceToEss 2)
                    , (TaskTime 2, DistanceToEss 1)
                    ]

    , HU.testCase "Three points, only 3rd moves further from goal = that point removed" $
        FS.cleanTrack (LcTrack [ (TaskTime 0, DistanceToEss 2)
                               , (TaskTime 1, DistanceToEss 1)
                               , (TaskTime 2, DistanceToEss 2)
                               ])
        @?= LcTrack [ (TaskTime 0, DistanceToEss 2)
                    , (TaskTime 1, DistanceToEss 1)
                    ]
    ]

distances :: LcTrack -> [Rational]
distances (LcTrack track) = (\(_, DistanceToEss d) -> d) <$> track

cleanTrack :: LcCleanTest -> Bool
cleanTrack (LcCleanTest track) =
    (\xs -> length xs <= (length $ distances track)) $ distances $ FS.cleanTrack track
