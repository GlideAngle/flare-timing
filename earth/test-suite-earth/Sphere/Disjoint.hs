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

module Sphere.Disjoint (disjointUnits) where

import Prelude hiding (span)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure ((+:), (*:), u, negate', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), Radius(..))
import Flight.Earth.Sphere (earthRadius)

import Zone (MkZone, QLL, showQ, describedZones)
import Sphere.Touching (Overlay(..), separatedZones)

disjointUnits :: TestTree
disjointUnits =
    testGroup "Disjoint zones are separated"
    $ ((uncurry f) <$> describedZones)
    where
        f s =
            distanceDisjoint
                (s ++ " zones")

disjoint :: String -> [[Zone Rational]] -> TestTree
disjoint = separatedZones Disjoint delta radius

delta :: (Real a, Fractional a) => Quantity a [u| 1 |]
delta = fromRational' . MkQuantity $ 1 % 100000000

radius :: (Real a, Fractional a) => Quantity a [u| 1 |]
radius = fromRational' . MkQuantity $ 2 % 1

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    [ ((z, z), (z, pos))
    , ((z, z), (z, neg))
    ]
    where
        z = [u| 0 rad |]

        pos = (radius +: delta) *: [u| 1 rad |]
        neg = negate' (radius +: delta) *: [u| 1 rad |]

distances :: (Real a, Fractional a) => [Radius a [u| m |]]
distances =
    repeat $ Radius earthRadius

distanceDisjoint
    :: String
    -> MkZone Double
    -> TestTree
distanceDisjoint s f =
    testGroup s
    $ zipWith
        (\r (x, y) ->
            disjoint
                (showQ x ++ " " ++ showQ y)
                [[f r x, f r y]])
        distances
        (pts :: [(QLL Double, QLL Double)])
