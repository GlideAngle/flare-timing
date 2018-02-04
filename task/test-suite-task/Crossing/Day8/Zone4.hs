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

module Crossing.Day8.Zone4 (units) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?>=), (@?<=))
import Data.UnitsOfMeasure (u, unQuantity)

import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import qualified Flight.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import EdgeToEdge (toLatLngDbl)

--     - - crossingPair:
--          - time: 2012-01-14T06:39:24Z
--            lat: -33.132598
--            lng: 147.57552599
--          - time: 2012-01-14T06:39:28Z
--            lat: -33.13195499
--            lng: 147.57574099
--          inZone:
--          - false
--          - true
--        - crossingPair:
--          - time: 2012-01-14T06:39:36Z
--            lat: -33.130946
--            lng: 147.576771
--          - time: 2012-01-14T06:39:40Z
--            lat: -33.130817
--            lng: 147.57756499
--          inZone:
--          - true
--          - false
units :: TestTree
units = testGroup "Task 8 Zone 4 Crossings"
    [ HU.testCase "dy1" $ unQuantity dy1 @?>= r
    , HU.testCase "dz1" $ unQuantity dz1 @?<= r

    , HU.testCase "dy2" $ unQuantity dy2 @?<= r
    , HU.testCase "dz2" $ unQuantity dz2 @?>= r
    ]
    where
        -- YARRAB
        r = unQuantity [u| 400m |]
        x = toLatLngDbl (negate 33.12908, 147.57322999)

        -- 2012-01-14T06:39:24/28
        y1 = toLatLngDbl (negate 33.132598, 147.57552599)
        z1 = toLatLngDbl (negate 33.13195499, 147.57574099)

        -- 2012-01-14T06:39:36/40
        y2 = toLatLngDbl (negate 33.130946, 147.576771)
        z2 = toLatLngDbl (negate 33.130817, 147.57756499)

        (TaskDistance dy1) = Dbl.distanceHaversine x y1
        (TaskDistance dz1) = Dbl.distanceHaversine x z1

        (TaskDistance dy2) = Dbl.distanceHaversine x y2
        (TaskDistance dz2) = Dbl.distanceHaversine x z2
