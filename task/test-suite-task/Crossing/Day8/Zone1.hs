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

module Crossing.Day8.Zone1 (units) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?>=), (@?<=))
import Data.UnitsOfMeasure (u, unQuantity)

import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import qualified Flight.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import EdgeToEdge (toLatLngDbl)

--    - - crossingPair:
--          - time: 2012-01-14T02:29:12Z
--            lat: -33.36088699
--            lng: 147.93120899
--          - time: 2012-01-14T02:29:16Z
--            lat: -33.360415
--            lng: 147.93073699
--          inZone:
--          - true
--          - false
--        - crossingPair:
--          - time: 2012-01-14T07:57:44Z
--            lat: -33.36019999
--            lng: 147.930651
--          - time: 2012-01-14T07:57:48Z
--            lat: -33.360951
--            lng: 147.931488
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - time: 2012-01-14T07:57:52Z
--            lat: -33.36168099
--            lng: 147.93226
--          - time: 2012-01-14T07:57:56Z
--            lat: -33.36219499
--            lng: 147.93298999
--          inZone:
--          - true
--          - false
units :: TestTree
units = testGroup "Task 8 Zone 1 Crossings"
    [ HU.testCase "dy1" $ unQuantity dy1 @?<= r
    , HU.testCase "dz1" $ unQuantity dz1 @?>= r

    , HU.testCase "dy2" $ unQuantity dy2 @?>= r
    , HU.testCase "dz2" $ unQuantity dz2 @?<= r

    , HU.testCase "dy3" $ unQuantity dy3 @?<= r
    , HU.testCase "dz3" $ unQuantity dz3 @?>= r
    ]
    where
        -- FORBES
        r = unQuantity [u| 100m |]
        x = toLatLngDbl (negate 33.36137, 147.93207)

        -- 2012-01-14T02:29:12/16
        y1 = toLatLngDbl (negate 33.36088699, 147.93120899)
        z1 = toLatLngDbl (negate 33.360415, 147.93073699)

        -- 2012-01-14T07:57:44/48
        y2 = toLatLngDbl (negate 33.36019999, 147.930651)
        z2 = toLatLngDbl (negate 33.360951, 147.931488)

        -- 2012-01-14T07:57:52/56
        y3 = toLatLngDbl (negate 33.36168099, 147.93226)
        z3 = toLatLngDbl (negate 33.36219499, 147.93298999)

        (TaskDistance dy1) = Dbl.distanceHaversine x y1
        (TaskDistance dz1) = Dbl.distanceHaversine x z1

        (TaskDistance dy2) = Dbl.distanceHaversine x y2
        (TaskDistance dz2) = Dbl.distanceHaversine x z2

        (TaskDistance dy3) = Dbl.distanceHaversine x y3
        (TaskDistance dz3) = Dbl.distanceHaversine x z3
