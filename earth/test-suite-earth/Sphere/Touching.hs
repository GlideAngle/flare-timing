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

module Sphere.Touching
    (Overlay(..), ZoneSpace(..)
    , separatedZones, touchingUnits
    ) where

import Prelude hiding (span)
import Data.Ratio ((%))
import Data.List (intersperse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure ((-:), (*:), u, convert, negate', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..), Radius(..), showZoneDMS, fromRationalZone)
import Flight.LatLng.Double (showAngle)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Flight.Earth.Sphere.Separated as S (separatedZones)
import Flight.Earth.Sphere (earthRadius)

import Zone (MkZone, QLL, showQ, dotZones, areaZones)

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

touchingUnits :: TestTree
touchingUnits =
    testGroup "Potentially overlapping zones are touching"
    [ testGroup "Zones without area are not touching" ((uncurry f) <$> dotZones)
    , testGroup "Zones with area are touching" ((uncurry g) <$> areaZones)
    ]
    where
        f s =
            zonesTouching
                (s ++ " zones")
                (separatedZones DotZone Overlap delta radius)

        g s =
            zonesTouching
                (s ++ " zones")
                (separatedZones AreaZone Overlap delta radius)

data Overlay = Overlap | Disjoint deriving (Eq, Show)
data ZoneSpace = DotZone | AreaZone deriving (Eq, Show)

separatedZones
    :: ZoneSpace
    -> Overlay
    -> Quantity Rational [u| 1 |]
    -> Quantity Rational [u| 1 |]
    -> String
    -> [[Zone Rational]]
    -> TestTree
separatedZones zt expected delta' radius' title xs =
    testGroup title (f <$> xs)
    where
        f x =
            testGroup sZone
                [ testGroup sRadius
                    [ HU.testCase sSep
                        $ S.separatedZones span x
                            @?= (expected == Disjoint || zt == DotZone)
                    ]
                ]
            where
                sRadius =
                    mconcat
                        [ "radius="
                        , showAngle radiusDMS
                        , " or "
                        , show radiusRad
                        ]

                sSep =
                    mconcat
                        [ if expected == Disjoint
                            then "separation="
                            else "overlap="
                        , showAngle deltaDMS
                        , " or "
                        , show deltaRad
                        ]

                sZone =
                    mconcat
                    $ intersperse ", " (showZoneDMS . fromRationalZone <$> x)

        deltaDMS :: Quantity Double [u| dms |]
        deltaDMS = fromRational' . convert $ delta' *: [u| 1 rad |]

        deltaRad :: Quantity Double [u| rad |]
        deltaRad = fromRational' $ delta' *: [u| 1 rad |]

        radiusDMS :: Quantity Double [u| dms |]
        radiusDMS = fromRational' . convert $ radius' *: [u| 1 rad |]

        radiusRad :: Quantity Double [u| rad |]
        radiusRad = fromRational' $ radius' *: [u| 1 rad |]

type Touching = String -> [[Zone Rational]] -> TestTree

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

        pos = (radius -: delta) *: [u| 1 rad |]
        neg = negate' (radius -: delta) *: [u| 1 rad |]

distances :: (Real a, Fractional a) => [Radius a [u| m |]]
distances =
    repeat $ Radius earthRadius

zonesTouching
    :: String
    -> Touching
    -> MkZone Double
    -> TestTree
zonesTouching s f g =
    testGroup s
    $ zipWith
        (\r (x, y) -> f (showQ x ++ " " ++ showQ y) [[g r x, g r y]])
        distances
        (pts :: [(QLL Double, QLL Double)])
