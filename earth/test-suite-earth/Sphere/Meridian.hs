{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Meridian (meridianUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure (u)

import Flight.Zone (Radius(..))
import Flight.Earth.Sphere (earthRadius)
import Sphere.Distance (toDistanceEqual)
import Zone (MkZone, QLL, describedZones, showQ)

meridianUnits :: TestTree
meridianUnits =
    testGroup "Meridian arc distance tests"
    $ ((uncurry f) <$> describedZones)
    where
        f s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    [ ((z, z), (z, [u| 1 rad |]))
    , ((z, z), (z, [u| -1 rad |]))
    ]
    where
        z = [u| 0 rad |]

distances :: [Radius Rational [u| m |]]
distances =
    Radius <$> repeat earthRadius

distanceMeridian
    :: String
    -> MkZone Double
    -> TestTree
distanceMeridian s f =
    testGroup s
    $ zipWith
        (\r@(Radius r') (x, y) ->
            toDistanceEqual
                r'
                (showQ x ++ " " ++ showQ y)
                (f r x, f r y))
        distances
        (pts :: [(QLL Double, QLL Double)])
