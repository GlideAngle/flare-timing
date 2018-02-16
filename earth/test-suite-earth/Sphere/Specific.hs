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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Specific (forbesUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, TestName, testGroup)
import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Forbes as F (mkDayUnits, mkPartDayUnits)
import Sphere.Forbes
    ( dd1, dd2, dd3, dd4, dd5, dd6, dd7, dd8
    , dsd1, dsd2, dsd3, dsd4, dsd5, dsd6, dsd7, dsd8
    )
import Forbes
    ( d1, d2, d3, d4, d5, d6, d7, d8
    , p1, p2, p3, p4, p5, p6, p7, p8
    )

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

mkDay
    :: TestName
    -> [Zone Rational]
    -> TaskDistance Rational
    -> [TaskDistance Rational]
    -> TestTree
mkDay = F.mkDayUnits (distancePointToPoint span)

mkPart
    :: TestName
    -> [Zone Rational]
    -> TaskDistance Rational
    -> TestTree
mkPart = F.mkPartDayUnits (distancePointToPoint span)

forbesUnits :: TestTree
forbesUnits =
    testGroup "Forbes 2011/2012 distances"
    [ mkDay "Task 1" d1 dd1 dsd1
    , p1 mkPart
        [u| 54.755578 km |]
        [u| 59.276627 km |]
        [u| 20.88547 km |]

    , mkDay "Task 2" d2 dd2 dsd2
    , p2 mkPart
        [u| 51.290669 km |]
        [u| 40.569544 km |]
        [u| 38.30752 km |]

    , mkDay "Task 3" d3 dd3 dsd3
    , p3 mkPart
        [u| 78.147093 km |]
        [u| 27.780099 km |]
        [u| 79.716223 km |]

    , mkDay "Task 4" d4 dd4 dsd4
    , p4 mkPart
        [u| 51.290669 km |]
        [u| 105.87255 km |]

    , mkDay "Task 5" d5 dd5 dsd5
    , p5 mkPart
        [u| 92.601904 km |]
        [u| 128.87562 km |]

    , mkDay "Task 6" d6 dd6 dsd6
    , p6 mkPart
        [u| 130.665489 km |]
        [u| 75.17947 km |]

    , mkDay "Task 7" d7 dd7 dsd7
    , p7 mkPart
        [u| 57.365312 km |]
        [u| 104.509732 km |]
        [u| 21.613886 km |]

    , mkDay "Task 8" d8 dd8 dsd8
    , p8 mkPart
        [u| 57.427511 km |]
        [u| 69.547668 km |]
        [u| 42.131961 km |]
    ]
