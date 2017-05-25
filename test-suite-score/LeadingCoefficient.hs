module LeadingCoefficient
    ( leadingCoefficientUnits
    , cleanTrack
    , leadingFractions
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
    , isNormal
    )

import TestNewtypes

leadingCoefficientUnits :: TestTree
leadingCoefficientUnits = testGroup "Leading coefficient unit tests"
    [ madeGoalUnits
    , cleanTrackUnits
    , coefficientUnits
    , leadingFractionsUnits
    ]

pt :: (Rational, Rational) -> (TaskTime, DistanceToEss)
pt (t, d) = (TaskTime t, DistanceToEss d)

pts :: [(Rational, Rational)] -> LcTrack
pts xs = LcTrack $ pt <$> xs

madeGoalUnits :: TestTree
madeGoalUnits = testGroup "Made goal unit tests"
    [ HU.testCase "Single point, at goal = made goal" $
        FS.madeGoal (pts [ (1, 0) ]) @?= True

    , HU.testCase "Single point, not at goal = didn't make goal" $
        FS.madeGoal (pts [ (1, 1) ]) @?= False

    , HU.testCase "Two points, not at goal = didn't make goal" $
        FS.madeGoal (pts [ (1, 2)
                         , (2, 1)
                         ]) @?= False

    , HU.testCase "Two points, only last point at goal = made goal" $
        FS.madeGoal (pts [ (1, 1)
                         , (2, 0)
                         ]) @?= True

    , HU.testCase "Two points, only first point at goal = made goal" $
        FS.madeGoal (pts [ (1, 0)
                         , (2, 1)
                         ]) @?= True
    ]

cleanTrackUnits :: TestTree
cleanTrackUnits = testGroup "Clean track unit tests"
    [ HU.testCase "Single point = no points removed" $
        FS.cleanTrack (pts [ (1, 0) ])
        @?= pts [ (1, 0) ]

    , HU.testCase "Two points, each one closer to goal = no points removed" $
        FS.cleanTrack (pts [ (1, 2)
                           , (2, 1)
                           ])
        @?= pts [ (1, 2) 
                , (2, 1)
                ]

    , HU.testCase "Two points, each one further from goal = all but one point removed" $
        FS.cleanTrack (pts [ (1, 1)
                           , (2, 2)
                           ])
        @?= pts [ (1, 1) ]

    , HU.testCase "Three points, each one closer to goal = no points removed" $
        FS.cleanTrack (pts [ (1, 3)
                           , (2, 2)
                           , (3, 1)
                           ])
        @?= pts [ (1, 3) 
                , (2, 2)
                , (3, 1)
                ]

    , HU.testCase "Three points, each one further from goal = all but one point removed" $
        FS.cleanTrack (pts [ (1, 1)
                           , (2, 2)
                           , (3, 3)
                           ])
        @?= pts [ (1, 1) ]

    , HU.testCase "Three points, only 2nd moves further from goal = that point removed" $
        FS.cleanTrack (pts [ (1, 2)
                           , (2, 3)
                           , (3, 1)
                           ])
        @?= pts [ (1, 2)
                , (3, 1)
                ]

    , HU.testCase "Three points, only 3rd moves further from goal = that point removed" $
        FS.cleanTrack (pts [ (1, 2)
                           , (2, 1)
                           , (3, 2)
                           ])
        @?= pts [ (1, 2)
                , (2, 1)
                ]
    ]

coefficientUnits :: TestTree
coefficientUnits = testGroup "Leading coefficient (LC) unit tests"
    [ HU.testCase "1 track point = 1 / 1800 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 1 % 1)
            (LengthOfSs $ 1)
            (pts [ (1, 0) ])
        @?= (LeadingCoefficient $ 1 % 1800)

    , HU.testCase "2 track points at SSS and ESS = 1 / 900 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 2 % 1)
            (LengthOfSs $ 1)
            (pts [ (1, 1)
                 , (2, 0)
                 ])
        @?= (LeadingCoefficient $ 1 % 900)

    , HU.testCase "3 track points evenly spread from SSS to ESS = 1 / 720 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 3 % 1)
            (LengthOfSs $ 2)
            (pts [ (1, 2)
                 , (2, 1)
                 , (3, 0)
                 ])
        @?= (LeadingCoefficient $ 1 % 720)

    , HU.testCase "4 track points evenly spread from SSS to ESS = 29 / 16200 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 4 % 1)
            (LengthOfSs $ 3)
            (pts [ (1, 3)
                 , (2, 2)
                 , (3, 1)
                 , (4, 0)
                 ])
        @?= (LeadingCoefficient $ 29 % 16200)

    , HU.testCase "5 track points evenly spread from SSS to ESS = 11 / 4800 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 5 % 1)
            (LengthOfSs $ 4)
            (pts [ (1, 4)
                 , (2, 3)
                 , (3, 2)
                 , (4, 1)
                 , (5, 0)
                 ])
        @?= (LeadingCoefficient $ 11 % 4800)

    , HU.testCase "5 track points cut short to 3 by a task deadline = 1 / 480 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 3 % 1)
            (LengthOfSs $ 4)
            (pts [ (1, 4)
                 , (2, 3)
                 , (3, 2)
                 , (4, 1)
                 , (5, 0)
                 ])
        @?= (LeadingCoefficient $ 1 % 480)

    , HU.testCase "5 track points with an equal distance flown before the speed section = 3 / 400 LC" $
        FS.leadingCoefficient
            (TaskDeadline $ 5 % 1)
            (LengthOfSs $ 2)
            (pts [ (1, 4)
                 , (2, 3)
                 , (3, 2)
                 , (4, 1)
                 , (5, 0)
                 ])
        @?= (LeadingCoefficient $ 3 % 400)
    ]

leadingFractionsUnits :: TestTree
leadingFractionsUnits = testGroup "Leading fractions unit tests"
    [ HU.testCase "2 pilots, identical tracks = 1 leading factor each" $
        FS.leadingFractions
            (TaskDeadline $ 5 % 1)
            (LengthOfSs $ 4)
            [ pts [ (1, 4)
                  , (2, 3)
                  , (3, 2)
                  , (4, 1)
                  , (5, 0)
                  ]
            , pts [ (1, 4)
                  , (2, 3)
                  , (3, 2)
                  , (4, 1)
                  , (5, 0)
                  ]
            ]
        @?= [ LeadingFraction $ 1 % 1
            , LeadingFraction $ 1 % 1
            ]
     
    , HU.testCase "2 pilots, one always leading = 0 & 1 leading factors" $
        FS.leadingFractions
            (TaskDeadline $ 5 % 1)
            (LengthOfSs $ 4)
            [ pts [ (1, 5)
                  , (2, 4)
                  , (3, 3)
                  , (4, 2)
                  , (5, 1)
                  ]
            , pts [ (1, 4)
                  , (2, 3)
                  , (3, 2)
                  , (4, 1)
                  , (5, 0)
                  ]
            ]
        @?= [ LeadingFraction $ 0 % 1
            , LeadingFraction $ 1 % 1
            ]
     
    , HU.testCase "2 pilots, one mostly leading = 0 & 1 leading factors" $
        FS.leadingFractions
            (TaskDeadline $ 5 % 1)
            (LengthOfSs $ 4)
            [ pts [ (1, 5)
                  , (2, 2)
                  , (3, 3)
                  , (4, 2)
                  , (5, 1)
                  ]
            , pts [ (1, 4)
                  , (2, 3)
                  , (3, 2)
                  , (4, 1)
                  , (5, 0)
                  ]
            ]
        @?= [ LeadingFraction $ 0 % 1
            , LeadingFraction $ 1 % 1
            ]
     
    , HU.testCase "2 pilots, alternating lead = 0 & 1 leading factors" $
        FS.leadingFractions
            (TaskDeadline $ 5 % 1)
            (LengthOfSs $ 8)
            [ pts [ (1, 8)
                  , (2, 6)
                  , (3, 5)
                  , (4, 2)
                  , (5, 1)
                  ]
            , pts [ (1, 8)
                  , (2, 7)
                  , (3, 4)
                  , (4, 3)
                  , (5, 0)
                  ]
            ]
        @?= [ LeadingFraction $ 0 % 1
            , LeadingFraction $ 1 % 1
            ]
    ]

isClean :: LcTrack -> LcTrack-> Bool
isClean rawTrack cleanedTrack =
    if any (< 1) ts
        then length ys < length xs
        else
            if xs == (reverse . sort $ xs)
               then length ys == length xs
               else length ys < length xs
    where
        ts = times rawTrack 
        xs = distances rawTrack
        ys = distances cleanedTrack

        times :: LcTrack -> [Rational]
        times (LcTrack track) = (\(TaskTime t, _) -> t) <$> track

        distances :: LcTrack -> [Rational]
        distances (LcTrack track) = (\(_, DistanceToEss d) -> d) <$> track

cleanTrack :: LcCleanTest -> Bool
cleanTrack (LcCleanTest track) =
    isClean track $ FS.cleanTrack track

leadingFractions :: LcTest -> Bool
leadingFractions (LcTest (deadline, lens, tracks)) =
    all (\(LeadingFraction x) -> isNormal x) $ FS.leadingFractions deadline lens tracks
