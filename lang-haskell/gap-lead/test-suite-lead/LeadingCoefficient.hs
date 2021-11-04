module LeadingCoefficient
    ( leadingCoefficientUnits
    , cleanTrack
    , leadingFractions
    ) where

import Data.List (sortBy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import "flight-gap-allot" Flight.Score (isNormal)
import qualified "flight-gap-lead" Flight.Score as FS
import "flight-gap-lead" Flight.Score
    ( TaskTime(..)
    , DistanceToEss(..)
    , LcTrack
    , Leg(..)
    , LcSeq(..)
    , LcPoint(..)
    --, TaskDeadline(..)
    , LengthOfSs(..)
    , LeadingFraction(..)
    )

import TestNewtypes

{-# ANN module ("HLint: ignore Use pragma syntax" :: String) #-}

leadingCoefficientUnits :: TestTree
leadingCoefficientUnits = testGroup "Leading coefficient unit tests"
    [ madeGoalUnits
    , cleanTrackUnits
    ]

pt :: (Rational, Rational) -> LcPoint
pt (t, d) =
    LcPoint
        { leg = RaceLeg 0
        , mark = TaskTime $ [u| s |] t
        , togo = DistanceToEss $ [u| km |] d
        }

pts :: [(Rational, Rational)] -> LcTrack
pts xs = LcSeq (pt <$> xs)

madeGoalUnits :: TestTree
madeGoalUnits = testGroup "Made goal unit tests"
    [ HU.testCase "Single point, at goal = made goal" $
        FS.madeGoal (pts [(1, 0)]) @?= True

    , HU.testCase "Single point, not at goal = didn't make goal" $
        FS.madeGoal (pts [(1, 1)]) @?= False

    , HU.testCase "Two points, not at goal = didn't make goal" $
        FS.madeGoal (pts [(1, 2), (2, 1)]) @?= False

    , HU.testCase "Two points, only last point at goal = made goal" $
        FS.madeGoal (pts [(1, 1), (2, 0)]) @?= True

    , HU.testCase "Two points, only first point at goal = made goal" $
        FS.madeGoal (pts [(1, 0), (2, 1)]) @?= True
    ]

cleanTrackUnits :: TestTree
cleanTrackUnits = testGroup "Clean track unit tests"
    [ HU.testCase "Single point = no points removed" $
        FS.cleanTrack (LengthOfSs [u| 1 km |]) (pts [ (1, 0) ])
        @?= pts [(1, 0)]

    , HU.testCase "Two points, each one closer to goal = no points removed" $
        FS.cleanTrack (LengthOfSs [u| 2 km |]) (pts [(1, 2), (2, 1)])
        @?= pts [(1, 2), (2, 1)]

    , HU.testCase "Two points, each one further from goal = all but one point removed" $
        FS.cleanTrack (LengthOfSs [u| 2 km |]) (pts [(1, 1), (2, 2)])
        @?= pts [(1, 1)]

    , HU.testCase "Three points, each one closer to goal = no points removed" $
        FS.cleanTrack (LengthOfSs [u| 3 km |]) (pts [(1, 3), (2, 2), (3, 1)])
        @?= pts [(1, 3), (2, 2), (3, 1)]

    , HU.testCase "Three points, each one further from goal = all but one point removed" $
        FS.cleanTrack (LengthOfSs [u| 2 km |]) (pts [(1, 1), (2, 2), (3, 3)])
        @?= pts [(1, 1)]

    , HU.testCase "Three points, only 2nd moves further from goal = that point removed" $
        FS.cleanTrack (LengthOfSs [u| 2 km |]) (pts [(1, 2), (2, 3), (3, 1)])
        @?= pts [(1, 2), (3, 1)]

    , HU.testCase "Three points, only 3rd moves further from goal = that point removed" $
        FS.cleanTrack (LengthOfSs [u| 2 km |]) (pts [(1, 2), (2, 1), (3, 2)])
        @?= pts [(1, 2), (2, 1)]
    ]

{-
WARNING: Leading has changed since these tests were last run.

import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

    [ madeGoalUnits
    , cleanTrackUnits
    , coefficientUnits
    , leadingFractionsUnits
    ]

coefficientUnits :: TestTree
coefficientUnits = testGroup "Leading coefficient (LC) unit tests"
    [ HU.testCase "1 track point = 0 LC" $
        FS.leadingCoefficient
            (TaskDeadline 1)
            (LengthOfSs 1)
            (pts [ (1, 0) ])
        @?= (LeadingCoefficient $ 0 % 1)

    , HU.testCase "2 track points at SSS and ESS = 1 / 900 LC" $
        FS.leadingCoefficient
            (TaskDeadline 2)
            (LengthOfSs 1)
            (pts [ (1, 1)
                 , (2, 0)
                 ])
        @?= (LeadingCoefficient $ 1 % 900)

    , HU.testCase "3 track points evenly spread from SSS to ESS = 1 / 800 LC" $
        FS.leadingCoefficient
            (TaskDeadline 3)
            (LengthOfSs 2)
            (pts [ (1, 2)
                 , (2, 1)
                 , (3, 0)
                 ])
        @?= (LeadingCoefficient $ 1 % 800)

    , HU.testCase "4 track points evenly spread from SSS to ESS = 23 / 16200 LC" $
        FS.leadingCoefficient
            (TaskDeadline 4)
            (LengthOfSs 3)
            (pts [ (1, 3)
                 , (2, 2)
                 , (3, 1)
                 , (4, 0)
                 ])
        @?= (LeadingCoefficient $ 23 % 16200)

    , HU.testCase "5 track points evenly spread from SSS to ESS = 23 / 14400 LC" $
        FS.leadingCoefficient
            (TaskDeadline 5)
            (LengthOfSs 4)
            (pts [ (1, 4)
                 , (2, 3)
                 , (3, 2)
                 , (4, 1)
                 , (5, 0)
                 ])
        @?= (LeadingCoefficient $ 23 % 14400)

    , HU.testCase "5 track points cut short to 3 by a task deadline = 29 / 28800 LC" $
        FS.leadingCoefficient
            (TaskDeadline 3)
            (LengthOfSs 4)
            (pts [ (1, 4)
                 , (2, 3)
                 , (3, 2)
                 , (4, 1)
                 , (5, 0)
                 ])
        @?= (LeadingCoefficient $ 29 % 28800)

    , HU.testCase "5 track points with an equal distance flown before the speed section = 23 / 3600 LC" $
        FS.leadingCoefficient
            (TaskDeadline 5)
            (LengthOfSs 2)
            (pts [ (1, 4)
                 , (2, 3)
                 , (3, 2)
                 , (4, 1)
                 , (5, 0)
                 ])
        @?= (LeadingCoefficient $ 23 % 3600)
    ]

leadingFractionsUnits :: TestTree
leadingFractionsUnits = testGroup "Leading fractions unit tests"
    [ HU.testCase "2 pilots, identical tracks = 1 leading factor each" $
        FS.leadingFractions
            (TaskDeadline 5)
            (LengthOfSs 4)
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
            (TaskDeadline 5)
            (LengthOfSs 4)
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
            (TaskDeadline 5)
            (LengthOfSs 4)
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
            (TaskDeadline 5)
            (LengthOfSs 8)
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

-}

leadingFractions :: LcTest -> Bool
leadingFractions (LcTest (lcMin, lc)) =
    (\(LeadingFraction x) -> isNormal x) $ FS.leadingFraction lcMin lc

isClean :: LengthOfSs -> LcTrack -> LcTrack-> Bool

isClean _ LcSeq{seq = []} _ =
    True

isClean
    (LengthOfSs len)
    rawTrack@LcSeq{seq = LcPoint{togo = DistanceToEss x} : _}
    cleanedTrack
    | any (< 1) ts || x > len || x < [u| 0 km |] = length ys < length xs
    | xs == sortBy (flip compare) xs = length ys == length xs
    | otherwise = length ys < length xs
    where
        ts = times rawTrack
        xs = distances rawTrack
        ys = distances cleanedTrack

        times :: LcTrack -> [Rational]
        times (LcSeq track) =
            (\LcPoint{mark = TaskTime (MkQuantity t)} -> t) <$> track

        distances :: LcTrack -> [Rational]
        distances LcSeq{seq = track} =
            (\LcPoint{togo = DistanceToEss (MkQuantity d)} -> d) <$> track

cleanTrack :: LcCleanTest -> Bool
cleanTrack (LcCleanTest (len, track)) =
    isClean len track $ FS.cleanTrack len track
