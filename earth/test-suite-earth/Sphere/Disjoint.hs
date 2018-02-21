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

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..), Radius(..))
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl
    (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat
    (distanceHaversine)
import qualified Flight.Earth.Sphere.Separated as S (separatedZones)
import Flight.Earth.Sphere (earthRadius)

import Zone (MkZone, QLL, showQ, dotZones, areaZones)
import Sphere.Touching (Overlay(..), separatedZones)

spanD :: SpanLatLng Double
spanD = Dbl.distanceHaversine

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

sepD :: [Zone Double] -> Bool
sepD = S.separatedZones spanD

sepR :: [Zone Rational] -> Bool
sepR = S.separatedZones spanR

disjointUnits :: TestTree
disjointUnits =
    testGroup "Disjoint zones are separated"
    [ testGroup "With doubles"
        $ ((uncurry fD) <$> dotZones)
        ++ ((uncurry gD) <$> areaZones)
    , testGroup "With rationals"
        $ ((uncurry fR) <$> dotZones)
        ++ ((uncurry gR) <$> areaZones)
    ]
    where
        fD s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepD Disjoint delta radius)

        gD s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepD Disjoint delta radius)

        fR s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepR Disjoint delta radius)

        gR s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepR Disjoint delta radius)

type Disjoint a = String -> [[Zone a]] -> TestTree

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

zonesDisjoint
    :: (Enum a, Real a, Fractional a)
    => String
    -> Disjoint a
    -> MkZone a a
    -> TestTree
zonesDisjoint s f g =
    testGroup s
    $ zipWith
        (\r (x, y) -> f (showQ x ++ " " ++ showQ y) [[g r x, g r y]])
        distances
        pts
