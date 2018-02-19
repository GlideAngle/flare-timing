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

module Sphere.Touching (Overlay(..), separatedZones, touchingUnits) where

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

import Zone (QLL, cylinder, line, conical, semicircle)

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

touchingUnits :: TestTree
touchingUnits =
    testGroup "Overlapping zones are touching"
    [ cylinderTouching
    , conicalTouching
    , lineTouching
    , semicircleTouching
    ]

data Overlay = Overlap | Disjoint deriving Eq

separatedZones
    :: Overlay
    -> Quantity Rational [u| 1 |]
    -> Quantity Rational [u| 1 |]
    -> String
    -> [[Zone Rational]]
    -> TestTree
separatedZones expected delta' radius' title xs =
    testGroup title (f <$> xs)
    where
        f x =
            testGroup sZone
                [ testGroup sRadius
                    [ HU.testCase sSep
                        $ S.separatedZones span x
                            @?= (expected == Disjoint)
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

touching :: String -> [[Zone Rational]] -> TestTree
touching = separatedZones Overlap delta radius

delta :: Quantity Rational [u| 1 |]
delta = MkQuantity $ 1 % 100000000

radius :: Quantity Rational [u| 1 |]
radius = MkQuantity $ 2 % 1

pts :: [(QLL Rational, QLL Rational)]
pts =
    [ ((z, z), (z, pos))
    , ((z, z), (z, neg))
    ]
    where
        z :: Quantity Rational [u| rad |]
        z = [u| 0 rad |]

        pos = (radius -: delta) *: [u| 1 rad |]
        neg = negate' (radius -: delta) *: [u| 1 rad |]

cylinderTouching :: TestTree
cylinderTouching =
    touching
    "Cylinder zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = cylinder $ Radius earthRadius

conicalTouching :: TestTree
conicalTouching =
    touching
    "Conical zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = conical $ Radius earthRadius

lineTouching :: TestTree
lineTouching =
    touching
    "Line zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = line $ Radius earthRadius

semicircleTouching :: TestTree
semicircleTouching =
    touching
    "Semicircle zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = semicircle $ Radius earthRadius
