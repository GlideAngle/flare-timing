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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Ellipsoid.Distance where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure (u, convert, zero, toRational')
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

type MkZone a = Real a => Radius Rational [u| m |] -> QLL a -> Zone Rational

describedZones
    :: Real a
    =>
        [
            ( String
            , Radius Rational [u| m |] -> QLL a -> Zone Rational
            )
        ]
describedZones =
    [ ("point", point)
    , ("vector", vector)
    , ("cylinder", cylinder)
    , ("conical", conical)
    , ("line", line)
    , ("semicircle", semicircle)
    ]

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
