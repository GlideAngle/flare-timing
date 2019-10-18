{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Crossing.Day8.Zone2 (units) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?>=), (@?<=))
import Data.UnitsOfMeasure (u, unQuantity)

import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import EdgeToEdge (toLatLngDbl)

--      zonesCrossNominees:
--      - - crossingPair:
--          - fix: 783
--            time: 2012-01-14T03:19:44Z
--            lat: -33.38912499
--            lng: 147.83009999
--          - fix: 784
--            time: 2012-01-14T03:19:48Z
--            lat: -33.38893199
--            lng: 147.82943499
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - fix: 809
--            time: 2012-01-14T03:21:28Z
--            lat: -33.37919
--            lng: 147.826431
--          - fix: 810
--            time: 2012-01-14T03:21:32Z
--            lat: -33.37869599
--            lng: 147.826431
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - fix: 811
--            time: 2012-01-14T03:21:36Z
--            lat: -33.37820299
--            lng: 147.826388
--          - fix: 812
--            time: 2012-01-14T03:21:40Z
--            lat: -33.377881
--            lng: 147.82613
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - fix: 817
--            time: 2012-01-14T03:22:00Z
--            lat: -33.379812
--            lng: 147.82645199
--          - fix: 818
--            time: 2012-01-14T03:22:04Z
--            lat: -33.379769
--            lng: 147.82711699
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - fix: 820
--            time: 2012-01-14T03:22:12Z
--            lat: -33.379319
--            lng: 147.826796
--          - fix: 821
--            time: 2012-01-14T03:22:16Z
--            lat: -33.379769
--            lng: 147.82649499
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - fix: 822
--            time: 2012-01-14T03:22:20Z
--            lat: -33.380349
--            lng: 147.826796
--          - fix: 823
--            time: 2012-01-14T03:22:24Z
--            lat: -33.38054199
--            lng: 147.827525
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - fix: 952
--            time: 2012-01-14T03:31:00Z
--            lat: -33.400283
--            lng: 147.8351
--          - fix: 953
--            time: 2012-01-14T03:31:04Z
--            lat: -33.400669
--            lng: 147.83445599
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - fix: 4812
--            time: 2012-01-14T07:48:20Z
--            lat: -33.28254499
--            lng: 147.87970999
--          - fix: 4813
--            time: 2012-01-14T07:48:24Z
--            lat: -33.28310299
--            lng: 147.87977499
--          inZone:
--          - false
--          - true
units :: TestTree
units = testGroup "Task 8 Zone 2 Crossings"
    [ HU.testCase "dy1" $ unQuantity dy1 @?<= r
    , HU.testCase "dz1" $ unQuantity dz1 @?>= r

    , HU.testCase "dy2" $ unQuantity dy2 @?>= r
    , HU.testCase "dz2" $ unQuantity dz2 @?<= r

    , HU.testCase "dy3" $ unQuantity dy3 @?<= r
    , HU.testCase "dz3" $ unQuantity dz3 @?>= r

    , HU.testCase "dy4" $ unQuantity dy4 @?>= r
    , HU.testCase "dz4" $ unQuantity dz4 @?<= r

    , HU.testCase "dy5" $ unQuantity dy5 @?<= r
    , HU.testCase "dz5" $ unQuantity dz5 @?>= r

    , HU.testCase "dy6" $ unQuantity dy6 @?>= r
    , HU.testCase "dz6" $ unQuantity dz6 @?<= r

    , HU.testCase "dy7" $ unQuantity dy7 @?<= r
    , HU.testCase "dz7" $ unQuantity dz7 @?>= r

    , HU.testCase "dy8" $ unQuantity dy8 @?>= r
    , HU.testCase "dz8" $ unQuantity dz8 @?<= r
    ]
    where
        -- FORBES
        r = unQuantity [u| 10000m |]
        x = toLatLngDbl (negate 33.36137, 147.93207)

        -- 2012-01-14T03:19:44/48 (true/false)
        y1 = toLatLngDbl (negate 33.38912499, 147.83009999)
        z1 = toLatLngDbl (negate 33.38893199, 147.82943499)

        -- 2012-01-14T03:21:28/32 (false/true)
        y2 = toLatLngDbl (negate 33.37919, 147.826431)
        z2 = toLatLngDbl (negate 33.37869599, 147.826431)

        -- 2012-01-14T03:21:36/40 (true/false)
        y3 = toLatLngDbl (negate 33.37820299, 147.826388)
        z3 = toLatLngDbl (negate 33.377881, 147.82613)

        -- 2012-01-14T03:22:00/04 (false/true)
        y4 = toLatLngDbl (negate 33.379812, 147.82645199)
        z4 = toLatLngDbl (negate 33.379769, 147.82711699)

        -- 2012-01-14T03:22:12/16 (true, false)
        y5 = toLatLngDbl (negate 33.379319, 147.826796)
        z5 = toLatLngDbl (negate 33.379769, 147.82649499)

        -- 2012-01-14T03:22:20/24 (false/true)
        y6 = toLatLngDbl (negate 33.380349, 147.826796)
        z6 = toLatLngDbl (negate 33.38054199, 147.827525)

        -- 2012-01-14T03:31:00/04 (true/false)
        y7 = toLatLngDbl (negate 33.400283, 147.8351)
        z7 = toLatLngDbl (negate 33.400669, 147.83445599)

        -- 2012-01-14T07:48:20/24 (false/true)
        y8 = toLatLngDbl (negate 33.28254499, 147.87970999)
        z8 = toLatLngDbl (negate 33.28310299, 147.87977499)

        (TaskDistance dy1) = Dbl.distanceHaversine x y1
        (TaskDistance dz1) = Dbl.distanceHaversine x z1

        (TaskDistance dy2) = Dbl.distanceHaversine x y2
        (TaskDistance dz2) = Dbl.distanceHaversine x z2

        (TaskDistance dy3) = Dbl.distanceHaversine x y3
        (TaskDistance dz3) = Dbl.distanceHaversine x z3

        (TaskDistance dy4) = Dbl.distanceHaversine x y4
        (TaskDistance dz4) = Dbl.distanceHaversine x z4

        (TaskDistance dy5) = Dbl.distanceHaversine x y5
        (TaskDistance dz5) = Dbl.distanceHaversine x z5

        (TaskDistance dy6) = Dbl.distanceHaversine x y6
        (TaskDistance dz6) = Dbl.distanceHaversine x z6

        (TaskDistance dy7) = Dbl.distanceHaversine x y7
        (TaskDistance dz7) = Dbl.distanceHaversine x z7

        (TaskDistance dy8) = Dbl.distanceHaversine x y8
        (TaskDistance dz8) = Dbl.distanceHaversine x z8
