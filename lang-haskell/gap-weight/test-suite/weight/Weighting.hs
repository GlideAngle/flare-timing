module Weighting
    ( weightingUnits
    , distanceWeight
    , arrivalWeight
    , leadingWeight
    , timeWeight
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import "flight-gap-base" Flight.Score (isNormal)
import qualified "flight-gap-weight" Flight.Score as FS
import "flight-gap-weight" Flight.Score
    ( Lw(..)
    , Aw(..)
    , GoalRatio(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    )
import TestNewtypes

weightingUnits :: TestTree
weightingUnits = testGroup "Weighting unit tests"
    [ HU.testCase "0 goal ratio = 0.9 distance weight" $
        FS.distanceWeight (GoalRatio 0) @?= DistanceWeight (9 % 10)

    , HU.testCase "Pg = 0 arrival weight" $
        FS.arrivalWeight AwZero @?= ArrivalWeight 0

    , HU.testCase "0 distance weight, Hg = 0.8 arrival by rank weight" $
        FS.arrivalWeight (AwHgRank (DistanceWeight 0)) @?= ArrivalWeight (1 % 8)

    , HU.testCase "1 distance weight, Hg = 0 arrival by rank weight" $
        FS.arrivalWeight (AwHgRank (DistanceWeight 1)) @?= ArrivalWeight 0

    , HU.testCase "0 distance weight, Hg = 0.8 arrival by time weight" $
        FS.arrivalWeight (AwHgTime (DistanceWeight 0)) @?= ArrivalWeight (1 % 4)

    , HU.testCase "1 distance weight, Hg = 0 arrival by time weight" $
        FS.arrivalWeight (AwHgTime (DistanceWeight 1)) @?= ArrivalWeight 0

    , HU.testCase "0 goal ratio, 0 distance ratio, Pg = 0 leading weight" $
        FS.leadingWeight (LwPgZ (DistanceRatio 0)) @?= LeadingWeight 0

    , HU.testCase "0 goal ratio, 1 distance ratio, Pg = 0.1 leading weight" $
        FS.leadingWeight (LwPgZ (DistanceRatio 1)) @?= LeadingWeight (1 % 10)

    , HU.testCase "0 distance weight, Pg = 0.35 leading weight" $
        FS.leadingWeight (LwPg (DistanceWeight 0)) @?= LeadingWeight (7 % 20)

    , HU.testCase "1 distance weight, Pg = 0 leading weight" $
        FS.leadingWeight (LwPg (DistanceWeight 1)) @?= LeadingWeight 0

    , HU.testCase "0 distance weight, Hg = 0.175 leading weight" $
        FS.leadingWeight (LwHg (DistanceWeight 0)) @?= LeadingWeight (7 % 40)

    , HU.testCase "1 distance weight, Hg = 0 leading weight" $
        FS.leadingWeight (LwHg (DistanceWeight 1)) @?= LeadingWeight 0

    , HU.testCase "0 distance weight, 0 leading weight, 0 arrival weight = 1 time weight" $
        FS.timeWeight
            (DistanceWeight 0)
            (LeadingWeight 0)
            (ArrivalWeight 0)
            @?=
            TimeWeight 1

    , HU.testCase "1 distance weight, 0 leading weight, 0 arrival weight = 0 time weight" $
        FS.timeWeight
            (DistanceWeight 1)
            (LeadingWeight 0)
            (ArrivalWeight 0)
            @?=
            TimeWeight 0

    , HU.testCase "0 distance weight, 1 leading weight, 0 arrival weight = 0 time weight" $
        FS.timeWeight
            (DistanceWeight 0)
            (LeadingWeight 1)
            (ArrivalWeight 0)
            @?=
            TimeWeight 0

    , HU.testCase "0 distance weight, 0 leading weight, 1 arrival weight = 0 time weight" $
        FS.timeWeight
            (DistanceWeight 0)
            (LeadingWeight 0)
            (ArrivalWeight 1)
            @?=
            TimeWeight 0
    ]

distanceWeight :: GrTest -> Bool
distanceWeight (GrTest gr) =
    (\(DistanceWeight w) -> isNormal w) $ FS.distanceWeight gr

arrivalWeight :: AwTest -> Bool
arrivalWeight (AwTest x) =
    (\(ArrivalWeight w) -> isNormal w) $ FS.arrivalWeight x

leadingWeight :: LwTest -> Bool
leadingWeight (LwTest x) =
    (\(LeadingWeight w) -> isNormal w) $ FS.leadingWeight x

timeWeight :: TwTest -> Bool
timeWeight (TwTest (d, l, a)) =
    (\(TimeWeight w) -> isNormal w) $ FS.timeWeight d l a
