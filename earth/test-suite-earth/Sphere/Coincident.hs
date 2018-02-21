{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Coincident (coincidentUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Radius(..))
import Flight.Zone.Path (distancePointToPoint)
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl
    (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat
    (distanceHaversine)
import Zone (MkZone, QLL, showQ, describedZones)
import qualified Distance as D (DistanceEqual, toDistanceEqual)

spanD :: SpanLatLng Double
spanD = Dbl.distanceHaversine

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

coincidentUnits :: TestTree
coincidentUnits =
    testGroup "Coincident zones unit tests"
    $ emptyDistance
    :
    [ testGroup "With doubles" $ ((uncurry f) <$> describedZones)
    , testGroup "With rationals" $ ((uncurry g) <$> describedZones)
    ]
    where
        f s =
            distanceZero
                ("Distance between coincident " ++ s ++ " zones")
                (D.toDistanceEqual spanD)

        g s =
            distanceZero
                ("Distance between coincident " ++ s ++ " zones")
                (D.toDistanceEqual spanR)

emptyDistance :: TestTree
emptyDistance =
    testGroup "Point-to-point distance"
    [ testCase "No zones = zero point-to-point distance" $
        edgesSum (distancePointToPoint spanR []) @?= (TaskDistance $ MkQuantity 0)
    ]

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    [ ((z, z), (z, z))
    , ((m, z), (m, z))
    , ((z, m), (z, m))
    , ((m, m), (m, m))
    ]
    where
        z = [u| 0 rad |]
        m = convert [u| 45 deg |]

distances :: (Real a, Fractional a) => [Radius a [u| m |]]
distances =
    repeat . Radius . fromRational' $ [u| 0 m |]

distanceZero
    :: (Enum a, Real a, Fractional a)
    => String
    -> D.DistanceEqual a
    -> MkZone a a
    -> TestTree
distanceZero s f g =
    testGroup s
    $ zipWith
        (\r@(Radius r') (x, y) ->
                f
                r'
                (showQ x ++ " " ++ showQ y)
                (g r x, g r y))
        distances
        pts
