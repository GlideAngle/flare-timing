{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Crossing.Day8.Zone5 (units) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?>=), (@?<=))
import Data.UnitsOfMeasure (u, unQuantity)

import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import EdgeToEdge (toLatLngDbl)

--      zonesCrossNominees:
--     - - crossingPair:
--          - time: 2012-01-14T02:29:32Z
--            lat: -33.35869799
--            lng: 147.928526
--          - time: 2012-01-14T02:29:36Z
--            lat: -33.35837599
--            lng: 147.92794699
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - time: 2012-01-14T02:31:56Z
--            lat: -33.36384799
--            lng: 147.928762
--          - time: 2012-01-14T02:32:00Z
--            lat: -33.36391199
--            lng: 147.929664
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - time: 2012-01-14T02:37:08Z
--            lat: -33.364556
--            lng: 147.93090799
--          - time: 2012-01-14T02:37:12Z
--            lat: -33.364727
--            lng: 147.931509
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - time: 2012-01-14T02:37:12Z
--            lat: -33.364727
--            lng: 147.931509
--          - time: 2012-01-14T02:37:16Z
--            lat: -33.364449
--            lng: 147.93185199
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - time: 2012-01-14T02:37:28Z
--            lat: -33.36453399
--            lng: 147.93075799
--          - time: 2012-01-14T02:37:32Z
--            lat: -33.365028
--            lng: 147.93108
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - time: 2012-01-14T02:37:36Z
--            lat: -33.364921
--            lng: 147.93159499
--          - time: 2012-01-14T02:37:40Z
--            lat: -33.36453399
--            lng: 147.93159499
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - time: 2012-01-14T02:37:44Z
--            lat: -33.364449
--            lng: 147.931101
--          - time: 2012-01-14T02:37:48Z
--            lat: -33.364899
--            lng: 147.930779
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - time: 2012-01-14T07:57:32Z
--            lat: -33.35788199
--            lng: 147.92816199
--          - time: 2012-01-14T07:57:36Z
--            lat: -33.35865499
--            lng: 147.92895599
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - time: 2012-01-14T07:58:12Z
--            lat: -33.36168099
--            lng: 147.93552199
--          - time: 2012-01-14T07:58:16Z
--            lat: -33.36122999
--            lng: 147.935886
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - time: 2012-01-14T07:58:32Z
--            lat: -33.35942699
--            lng: 147.93552199
--          - time: 2012-01-14T07:58:36Z
--            lat: -33.35921299
--            lng: 147.935007
--          inZone:
--          - false
--          - true
units :: TestTree
units = testGroup "Task 8 Zone 5 Crossings"
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

    , HU.testCase "dy9" $ unQuantity dy9 @?<= r
    , HU.testCase "dz9" $ unQuantity dz9 @?>= r

    , HU.testCase "dy10" $ unQuantity dy10 @?>= r
    , HU.testCase "dz10" $ unQuantity dz10 @?<= r
    ]
    where
        -- DAY8GO
        r = unQuantity [u| 400m |]
        x = toLatLngDbl (negate 33.36099999, 147.93149999)

        -- 2012-01-14T02:29:32/36 (true/false)
        y1 = toLatLngDbl (negate 33.35869799, 147.928526)
        z1 = toLatLngDbl (negate 33.35837599, 147.92794699)

        -- 2012-01-14T02:31:56/32:00 (false/true)
        y2 = toLatLngDbl (negate 33.36384799, 147.928762)
        z2 = toLatLngDbl (negate 33.36391199, 147.929664)

        -- 2012-01-14T02:37:08/12 (true/false)
        y3 = toLatLngDbl (negate 33.364556, 147.93090799)
        z3 = toLatLngDbl (negate 33.364727, 147.931509)

        -- 2012-01-14T02:37:12/16 (false/true)
        y4 = toLatLngDbl (negate 33.364727, 147.931509)
        z4 = toLatLngDbl (negate 33.364449, 147.93185199)

        -- 2012-01-14T02:37:28/32 (true/false)
        y5 = toLatLngDbl (negate 33.36453399, 147.93075799)
        z5 = toLatLngDbl (negate 33.365028, 147.93108)

        -- 2012-01-14T02:37:36/40  (false/true)
        y6 = toLatLngDbl (negate 33.364921, 147.93159499)
        z6 = toLatLngDbl (negate 33.36453399, 147.93159499)

        -- 2012-01-14T02:37:44/48 (true/false)
        y7 = toLatLngDbl (negate 33.364449, 147.931101)
        z7 = toLatLngDbl (negate 33.364899, 147.930779)

        -- 2012-01-14T07:57:32/36 (false/true)
        y8 = toLatLngDbl (negate 33.35788199, 147.92816199)
        z8 = toLatLngDbl (negate 33.35865499, 147.92895599)

        -- 2012-01-14T07:58:12/16 (true/false)
        y9 = toLatLngDbl (negate 33.36168099, 147.93552199)
        z9 = toLatLngDbl (negate 33.36122999, 147.935886)

        -- 2012-01-14T07:58:32/36 (false/true)
        y10 = toLatLngDbl (negate 33.35942699, 147.93552199)
        z10 = toLatLngDbl (negate 33.35921299, 147.935007)

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

        (TaskDistance dy9) = Dbl.distanceHaversine x y9
        (TaskDistance dz9) = Dbl.distanceHaversine x z9

        (TaskDistance dy10) = Dbl.distanceHaversine x y10
        (TaskDistance dz10) = Dbl.distanceHaversine x z10
