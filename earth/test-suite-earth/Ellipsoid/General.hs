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

module Ellipsoid.General (zoneUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure ((*:), u, convert, zero, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , Incline (..)
    , Bearing(..)
    )
import Flight.Zone.Path (distancePointToPoint)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat
    (distanceVincenty)
import Flight.Earth.Ellipsoid (wgs84)
import Tolerance (diff, showTolerance)
import DegMinSec (fromQ)

span :: SpanLatLng Rational
span = Rat.distanceVincenty defEps wgs84

type QLL a = (Quantity a [u| rad |], Quantity a [u| rad |])

showQ :: QLL Double -> String
showQ (x, y) =
    show (fromQ x, fromQ y)

toLL :: Real a => QLL a -> LatLng Rational [u| rad |]
toLL (lat, lng) =
    LatLng (Lat lat', Lng lng')
    where
        lat' = toRational' lat
        lng' = toRational' lng

type MkZone a = Real a => Radius Rational [u| m |] -> QLL a -> Zone Rational

point :: MkZone a
point _ x = Point $ toLL x

vector :: MkZone a
vector _ x = Vector (Bearing zero) (toLL x) 

cylinder :: MkZone a
cylinder r x = Cylinder r (toLL x)

conical :: MkZone a
conical r x = Conical (Incline $ MkQuantity 1) r (toLL x)

line :: MkZone a
line r x = Line r (toLL x) 

semicircle :: MkZone a
semicircle r x = SemiCircle r (toLL x)

zoneUnits :: TestTree
zoneUnits =
    testGroup "Zone unit tests"
    [ distanceUnits
    ]

distanceUnits :: TestTree
distanceUnits =
    testGroup "Point-to-point distance"
    [ emptyDistance
    , pointDistanceZero
    , pointDistanceMeridian
    , vectorDistanceZero
    , vectorDistanceMeridian
    , cylinderDistanceZero
    , cylinderDistanceMeridian
    , conicalDistanceZero
    , conicalDistanceMeridian
    , lineDistanceZero
    , lineDistanceMeridian
    , semicircleDistanceZero
    , semicircleDistanceMeridian
    ]

emptyDistance :: TestTree
emptyDistance =
    testGroup "Point-to-point distance"
    [ testCase "No zones = zero point-to-point distance" $
        edgesSum (distancePointToPoint span []) @?= (TaskDistance $ MkQuantity 0)
    ]

toDistanceEqual
    :: Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceEqual expected title xy =
    testGroup title (f xy)
    where
        f (x, y) =
            [ testCase
                ( "within ± "
                ++ showTolerance tolerance'
                ++ " of expected "
                ++ show (TaskDistance expected)
                )
                $ found @?= (TaskDistance expected)
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

        tolerance :: Quantity Double [u| mm |]
        tolerance = [u| 2.8 mm |]

        tolerance' = convert tolerance

toDistanceClose
    :: Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceClose expected title xy =
    testGroup title (f xy)
    where
        f (x, y) =
            [ testCase
                ( "within ± "
                ++ showTolerance tolerance'
                ++ " of expected "
                ++ show (TaskDistance expected)
                )
                $ diff found (TaskDistance expected)
                @?<= (TaskDistance . toRational' $ tolerance')
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

        tolerance :: Quantity Double [u| mm |]
        tolerance = [u| 2.8 mm |]

        tolerance' = convert tolerance

ptsDistanceZero :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
ptsDistanceZero =
    [ ((z, z), (z, z))
    , ((m, z), (m, z))
    , ((z, m), (z, m))
    , ((m, m), (m, m))
    ]
    where
        z = [u| 0 rad |]
        m = convert [u| 45 deg |]

ptsDistanceMeridian :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
ptsDistanceMeridian =
    meridianArc . convert
    <$> [ x *: [u| 1 deg |] | x <- [5, 10 .. 90]]
    where
        meridianArc d =
            (([u| 0 rad |], [u| 0 rad |]), (d, [u| 0 rad |]))

ptsRadiiZero :: [Radius Rational [u| m |]]
ptsRadiiZero =
    Radius <$> replicate 4 [u| 0 m |]

ptsRadiiMeridian :: [Radius Rational [u| m |]]
ptsRadiiMeridian =
    Radius
    <$>
    [ [u| 552885.45156 m |]
    , [u| 1105854.83418 m |]
    , [u| 1658989.59067 m |]
    , [u| 2212366.25562 m |]
    , [u| 2766054.17059 m |]
    , [u| 3320113.39921 m |]
    , [u| 3874592.90264 m |]
    , [u| 4429529.03085 m |]
    , [u| 4984944.37798 m |]
    , [u| 5540847.04118 m |]
    , [u| 6097230.31218 m |]
    , [u| 6654072.81821 m |]
    , [u| 7211339.11585 m |]
    , [u| 7768980.72630 m |]
    , [u| 8326937.59000 m |]
    , [u| 8885139.87094 m |]
    , [u| 9443510.14009 m |]
    , [u| 10001965.72922 m |]
    ]

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
        ptsRadiiZero
        (ptsDistanceZero :: [(QLL Double, QLL Double)])

pointDistanceZero :: TestTree
pointDistanceZero =
    distanceZero
        "Distance between coincident point zones"
        point

vectorDistanceZero :: TestTree
vectorDistanceZero =
    distanceZero
        "Distance between coincident vector zones"
        vector

cylinderDistanceZero :: TestTree
cylinderDistanceZero =
    distanceZero
        "Distance between coincident cylinder zones"
        cylinder

conicalDistanceZero :: TestTree
conicalDistanceZero =
    distanceZero
        "Distance between coincident conical zones"
        conical

lineDistanceZero :: TestTree
lineDistanceZero =
    distanceZero
        "Distance between coincident line zones"
        line

semicircleDistanceZero :: TestTree
semicircleDistanceZero =
    distanceZero
        "Distance between coincident semicircle zones"
        semicircle

distanceMeridian
    :: String
    -> MkZone Double
    -> TestTree
distanceMeridian s f =
    testGroup s
    $ zipWith
        (\r@(Radius r') (x, y) ->
            toDistanceClose
                r'
                (showQ x ++ " " ++ showQ y)
                (f r x, f r y))
        ptsRadiiMeridian
        (ptsDistanceMeridian :: [(QLL Double, QLL Double)])

pointDistanceMeridian :: TestTree
pointDistanceMeridian =
    distanceMeridian
        "Distance between point zones on meridians"
        point

vectorDistanceMeridian :: TestTree
vectorDistanceMeridian =
    distanceMeridian
        "Distance between vector zones on meridians"
        vector

cylinderDistanceMeridian :: TestTree
cylinderDistanceMeridian =
    distanceMeridian
        "Distance between cylinder zones on meridians"
        cylinder

conicalDistanceMeridian :: TestTree
conicalDistanceMeridian =
    distanceMeridian
        "Distance between conical zones on meridians"
        conical

lineDistanceMeridian :: TestTree
lineDistanceMeridian =
    distanceMeridian
        "Distance between line zones on meridians"
        line

semicircleDistanceMeridian :: TestTree
semicircleDistanceMeridian =
    distanceMeridian
        "Distance between semicircle zones on meridians"
        semicircle
