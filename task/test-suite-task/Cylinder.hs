{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cylinder (cylinderUnits) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?>=), (@?<=))
import Data.UnitsOfMeasure (u, unQuantity)

import qualified Flight.PointToPoint.Double as Dbl (distanceHaversine)
import Flight.Units ()
import Flight.Task (TaskDistance(..))
import EdgeToEdge (toLatLngDbl)

cylinderUnits :: TestTree
cylinderUnits = testGroup "Zone edge shortest path unit tests"
    [ forbesUnits
    ]


forbesUnits :: TestTree
forbesUnits = testGroup "Forbes 2011/2012 distances"
    [ day8CrossingUnits
    ]

--  - - crossingPair:
--       - time: 2012-01-14T03:18:20Z
--         lat: -33.393438
--         lng: 147.842181
--       - time: 2012-01-14T03:18:24Z
--         lat: -33.39337299
--         lng: 147.841516
--       inZone:
--       - true
--       - false
--     - crossingPair:
--       - time: 2012-01-14T03:25:40Z
--         lat: -33.38309499
--         lng: 147.841687
--       - time: 2012-01-14T03:25:44Z
--         lat: -33.38309499
--         lng: 147.842374
--       inZone:
--       - false
--       - true
--     - crossingPair:
--       - time: 2012-01-14T03:29:04Z
--         lat: -33.38489799
--         lng: 147.84261
--       - time: 2012-01-14T03:29:08Z
--         lat: -33.38491899
--         lng: 147.842095
--       inZone:
--       - true
--       - false
--     - crossingPair:
--       - time: 2012-01-14T07:48:20Z
--         lat: -33.28254499
--         lng: 147.87970999
--       - time: 2012-01-14T07:48:24Z
--         lat: -33.28310299
--         lng: 147.87977499
--       inZone:
--       - false
--       - true
day8CrossingUnits :: TestTree
day8CrossingUnits = testGroup "Task 8 Zone Crossings"
    [ HU.testCase "dy1" $ unQuantity dy1 @?<= r
    , HU.testCase "dz1" $ unQuantity dz1 @?>= r

    , HU.testCase "dy2" $ unQuantity dy2 @?<= r
    , HU.testCase "dz2" $ unQuantity dz2 @?>= r

    , HU.testCase "dy3" $ unQuantity dy3 @?<= r
    , HU.testCase "dz3" $ unQuantity dz3 @?>= r

    , HU.testCase "dy3" $ unQuantity dy4 @?<= r
    , HU.testCase "dz3" $ unQuantity dz4 @?>= r

    ]
    where
        r = unQuantity [u| 10000m |]
        x = toLatLngDbl (negate 33.36137, 147.93207)

        -- 2012-01-14T03:18:20/24
        y1 = toLatLngDbl (negate 33.393438, 147.842181)
        z1 = toLatLngDbl (negate 33.39337299, 147.841516)

        -- 2012-01-14T03:25:40/44
        y2 = toLatLngDbl (negate 33.38309499, 147.841687)
        z2 = toLatLngDbl (negate 33.38309499, 147.842374)

        -- 2012-01-14T03:29:04/08
        y3 = toLatLngDbl (negate 33.38489799, 147.84261)
        -- 2012-01-14T03:29:08Z
        z3 = toLatLngDbl (negate 33.38491899, 147.842095)

        -- 2012-01-14T07:48:20/24
        y4 = toLatLngDbl (negate 33.28254499, 147.87970999)
        z4 = toLatLngDbl (negate 33.28310299, 147.87977499)

        (TaskDistance dy1) = Dbl.distanceHaversine x y1
        (TaskDistance dz1) = Dbl.distanceHaversine x z1

        (TaskDistance dy2) = Dbl.distanceHaversine x y2
        (TaskDistance dz2) = Dbl.distanceHaversine x z2

        (TaskDistance dy3) = Dbl.distanceHaversine x y3
        (TaskDistance dz3) = Dbl.distanceHaversine x z3

        (TaskDistance dy4) = Dbl.distanceHaversine x y4
        (TaskDistance dz4) = Dbl.distanceHaversine x z4
