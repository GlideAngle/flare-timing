module LeadingCoefficient
    ( leadingCoefficientUnits
    , cleanTrack
    ) where

import Data.List (sort, reverse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( TaskTime(..)
    , DistanceToEss(..)
    , LcTrack(..)
    , TaskDeadline(..)
    , LengthOfSs(..)
    , LeadingCoefficient(..)
    , LeadingFraction(..)
    )

import TestNewtypes

leadingCoefficientUnits :: TestTree
leadingCoefficientUnits = testGroup "Leading coefficient unit tests"
    [ madeGoalUnits
    , cleanTrackUnits
    , coefficientUnits
    , leadingFractionsUnits
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

coefficientUnits :: TestTree
coefficientUnits = testGroup "Leading coefficient (LC) unit tests"
    [ HU.testCase "1 track point = 0 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 1 % 1)
            (LengthOfSs $ 1)
            (LcTrack [ (TaskTime 0, DistanceToEss 0) ])
        @?= LeadingCoefficient 0

    , HU.testCase "2 track points at SSS and ESS = 0 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 1 % 1)
            (LengthOfSs $ 1)
            (LcTrack [ (TaskTime 0, DistanceToEss 1)
                     , (TaskTime 1, DistanceToEss 0)
                     ])
        @?= LeadingCoefficient 0

    , HU.testCase "3 track points evenly spread from SSS to ESS = 1 / 7200 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 2 % 1)
            (LengthOfSs $ 2)
            (LcTrack [ (TaskTime 0, DistanceToEss 2)
                     , (TaskTime 1, DistanceToEss 1)
                     , (TaskTime 2, DistanceToEss 0)
                     ])
        @?= (LeadingCoefficient $ 1 % 7200)

    , HU.testCase "4 track points evenly spread from SSS to ESS = 1 / 2700 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 3 % 1)
            (LengthOfSs $ 3)
            (LcTrack [ (TaskTime 0, DistanceToEss 3)
                     , (TaskTime 1, DistanceToEss 2)
                     , (TaskTime 2, DistanceToEss 1)
                     , (TaskTime 3, DistanceToEss 0)
                     ])
        @?= (LeadingCoefficient $ 1 % 2700)

    , HU.testCase "5 track points evenly spread from SSS to ESS = 1 / 1440 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 4 % 1)
            (LengthOfSs $ 4)
            (LcTrack [ (TaskTime 0, DistanceToEss 4)
                     , (TaskTime 1, DistanceToEss 3)
                     , (TaskTime 2, DistanceToEss 2)
                     , (TaskTime 3, DistanceToEss 1)
                     , (TaskTime 4, DistanceToEss 0)
                     ])
        @?= (LeadingCoefficient $ 1 % 1440)

    , HU.testCase "5 track points cut short to 3 by a task deadline = 7 / 14400 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 2 % 1)
            (LengthOfSs $ 4)
            (LcTrack [ (TaskTime 0, DistanceToEss 4)
                     , (TaskTime 1, DistanceToEss 3)
                     , (TaskTime 2, DistanceToEss 2)
                     , (TaskTime 3, DistanceToEss 1)
                     , (TaskTime 4, DistanceToEss 0)
                     ])
        @?= (LeadingCoefficient $ 7 % 14400)

    , HU.testCase "5 track points with an equal distance flown before the speed section = 1 / 360 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 4 % 1)
            (LengthOfSs $ 2)
            (LcTrack [ (TaskTime 0, DistanceToEss 4)
                     , (TaskTime 1, DistanceToEss 3)
                     , (TaskTime 2, DistanceToEss 2)
                     , (TaskTime 3, DistanceToEss 1)
                     , (TaskTime 4, DistanceToEss 0)
                     ])
        @?= (LeadingCoefficient $ 1 % 360)
    ]

leadingFractionsUnits :: TestTree
leadingFractionsUnits = testGroup "Leading fractions unit tests"
    [ HU.testCase "2 pilots, identical tracks = 1 leading factor each" $
        FS.leadingFractions
            (TaskDeadline $ 4 % 1)
            (LengthOfSs $ 4)
            [ LcTrack [ (TaskTime 0, DistanceToEss 4)
                      , (TaskTime 1, DistanceToEss 3)
                      , (TaskTime 2, DistanceToEss 2)
                      , (TaskTime 3, DistanceToEss 1)
                      , (TaskTime 4, DistanceToEss 0)
                      ]
            , LcTrack [ (TaskTime 0, DistanceToEss 4)
                      , (TaskTime 1, DistanceToEss 3)
                      , (TaskTime 2, DistanceToEss 2)
                      , (TaskTime 3, DistanceToEss 1)
                      , (TaskTime 4, DistanceToEss 0)
                      ]
            ]
        @?= [ LeadingFraction $ 1 % 1
            , LeadingFraction $ 1 % 1
            ]
     
    , HU.testCase "2 pilots, one always leading = 0 & 1 leading factors" $
        FS.leadingFractions
            (TaskDeadline $ 4 % 1)
            (LengthOfSs $ 4)
            [ LcTrack [ (TaskTime 0, DistanceToEss 5)
                      , (TaskTime 1, DistanceToEss 4)
                      , (TaskTime 2, DistanceToEss 3)
                      , (TaskTime 3, DistanceToEss 2)
                      , (TaskTime 4, DistanceToEss 1)
                      ]
            , LcTrack [ (TaskTime 0, DistanceToEss 4)
                      , (TaskTime 1, DistanceToEss 3)
                      , (TaskTime 2, DistanceToEss 2)
                      , (TaskTime 3, DistanceToEss 1)
                      , (TaskTime 4, DistanceToEss 0)
                      ]
            ]
        @?= [ LeadingFraction $ 0 % 1
            , LeadingFraction $ 1 % 1
            ]
     
    , HU.testCase "2 pilots, one mostly leading = 0 & 1 leading factors" $
        FS.leadingFractions
            (TaskDeadline $ 4 % 1)
            (LengthOfSs $ 4)
            [ LcTrack [ (TaskTime 0, DistanceToEss 5)
                      , (TaskTime 1, DistanceToEss 2)
                      , (TaskTime 2, DistanceToEss 3)
                      , (TaskTime 3, DistanceToEss 2)
                      , (TaskTime 4, DistanceToEss 1)
                      ]
            , LcTrack [ (TaskTime 0, DistanceToEss 4)
                      , (TaskTime 1, DistanceToEss 3)
                      , (TaskTime 2, DistanceToEss 2)
                      , (TaskTime 3, DistanceToEss 1)
                      , (TaskTime 4, DistanceToEss 0)
                      ]
            ]
        @?= [ LeadingFraction $ 0 % 1
            , LeadingFraction $ 1 % 1
            ]
     
    , HU.testCase "2 pilots, alternating lead = 0 & 1 leading factors" $
        FS.leadingFractions
            (TaskDeadline $ 4 % 1)
            (LengthOfSs $ 8)
            [ LcTrack [ (TaskTime 0, DistanceToEss 8)
                      , (TaskTime 1, DistanceToEss 6)
                      , (TaskTime 2, DistanceToEss 5)
                      , (TaskTime 3, DistanceToEss 2)
                      , (TaskTime 4, DistanceToEss 1)
                      ]
            , LcTrack [ (TaskTime 0, DistanceToEss 8)
                      , (TaskTime 1, DistanceToEss 7)
                      , (TaskTime 2, DistanceToEss 4)
                      , (TaskTime 3, DistanceToEss 3)
                      , (TaskTime 4, DistanceToEss 0)
                      ]
            ]
        @?= [ LeadingFraction $ 0 % 1
            , LeadingFraction $ 1 % 1
            ]
    ]

distances :: LcTrack -> [Rational]
distances (LcTrack track) = (\(_, DistanceToEss d) -> d) <$> track

isRevSorted :: Ord a => [a] -> Bool
isRevSorted xs =
    xs ==  (reverse . sort $ xs)

isClean :: LcTrack -> LcTrack-> Bool
isClean rawTrack cleanedTrack =
    if isRevSorted xs then
        length ys == length xs
    else
        length ys < length xs
    where
        xs = distances rawTrack
        ys = distances cleanedTrack

cleanTrack :: LcCleanTest -> Bool
cleanTrack (LcCleanTest track) =
    isClean track $ FS.cleanTrack track
