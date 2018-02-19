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
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone (Radius(..))
import Flight.Zone.Path (distancePointToPoint)
import Sphere.Distance (span, toDistanceEqual)
import Zone (MkZone, QLL, showQ, describedZones)

coincidentUnits :: TestTree
coincidentUnits =
    testGroup "Coincident zones unit tests"
    $ emptyDistance
    : ((uncurry f) <$> describedZones)
    where
        f s =
            distanceZero
                ("Distance between coincident " ++ s ++ " zones")

emptyDistance :: TestTree
emptyDistance =
    testGroup "Point-to-point distance"
    [ testCase "No zones = zero point-to-point distance" $
        edgesSum (distancePointToPoint span []) @?= (TaskDistance $ MkQuantity 0)
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

distances :: [Radius Rational [u| m |]]
distances =
    Radius <$> replicate 4 [u| 0 m |]

distanceZero
    :: String
    -> MkZone Double
    -> TestTree
distanceZero s f =
    testGroup s
    $ zipWith
        (\r@(Radius r') (x, y) ->
            toDistanceEqual
                r'
                (showQ x ++ " " ++ showQ y)
                (f r x, f r y))
        distances
        (pts :: [(QLL Double, QLL Double)])
