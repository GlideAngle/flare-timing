{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Ellipsoid.FsAndoyer.Forbes (units) where

import Test.Tasty (TestTree, TestName, testGroup)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Distance (QTaskDistance)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import qualified Forbes as F
    ( mkDayUnits, mkPartDayUnits
    , d1, d2, d3, d4, d5, d6, d7, d8
    , p1, p2, p3, p4, p5, p6, p7, p8
    )
import ToLatLng (toLatLngR)
import Flight.Earth.Ellipsoid (wgs84)
import Ellipsoid.FsAndoyer.Span (spanR)

d1, d2, d3, d4, d5, d6, d7, d8 :: [Zone Rational]
d1 = F.d1 toLatLngR
d2 = F.d2 toLatLngR
d3 = F.d3 toLatLngR
d4 = F.d4 toLatLngR
d5 = F.d5 toLatLngR
d6 = F.d6 toLatLngR
d7 = F.d7 toLatLngR
d8 = F.d8 toLatLngR

p1 = F.p1 toLatLngR
p2 = F.p2 toLatLngR
p3 = F.p3 toLatLngR
p4 = F.p4 toLatLngR
p5 = F.p5 toLatLngR
p6 = F.p6 toLatLngR
p7 = F.p7 toLatLngR
p8 = F.p8 toLatLngR

mkDay
    :: TestName
    -> [Zone Rational]
    -> Quantity Rational [u| km |]
    -> [Quantity Rational [u| km |]]
    -> TestTree
mkDay = F.mkDayUnits (distancePointToPoint $ spanR wgs84)

mkPart
    :: TestName
    -> [Zone Rational]
    -> QTaskDistance Rational [u| m |]
    -> TestTree
mkPart = F.mkPartDayUnits (distancePointToPoint $ spanR wgs84)

units :: TestTree
units =
    testGroup "Forbes 2011/2012 distances"
    [ mkDay "Task 1" d1
        [u| 134.950378 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 54.744775 km |]
        , [u| 114.065177 km |]
        , [u| 134.950378 km |]
        ]

    , p1 mkPart
        [u| 54.744775 km |]
        [u| 59.320403 km |]
        [u| 20.8852 km |]

    , mkDay "Task 2" d2
        [u| 130.298422 km |]
        [ [u| 0.0 km |]
        , [u| 4.9 km |]
        , [u| 51.279001 km |]
        , [u| 91.93244 km |]
        , [u| 130.298422 km |]
        ]

    , p2 mkPart
        [u| 51.279001 km |]
        [u| 40.653439 km |]
        [u| 38.365983 km |]

    , mkDay "Task 3" d3
        [u| 185.699509 km |]
        [ [u| 0.0 km |]
        , [u| 24.9 km |]
        , [u| 78.154142 km |]
        , [u| 105.985017 km |]
        , [u| 185.699509 km |]
        ]

    , p3 mkPart
        [u| 78.154142 km |]
        [u| 27.830875 km |]
        [u| 79.714492 km |]

    , mkDay "Task 4" d4
        [u| 157.320241 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 51.279001 km |]
        , [u| 157.320241 km |]
        ]

    , p4 mkPart
        [u| 51.279001 km |]
        [u| 106.041241 km |]

    , mkDay "Task 5" d5
        [u| 221.670332 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 92.596596 km |]
        , [u| 221.670332 km |]
        ]

    , p5 mkPart
        [u| 92.596596 km |]
        [u| 129.073735 km |]

    , mkDay "Task 6" d6
        [u| 205.866202 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 130.632578 km |]
        , [u| 205.866202 km |]
        ]

    , p6 mkPart
        [u| 130.632578 km |]
        [u| 75.233623 km |]

    , mkDay "Task 7" d7
        [u| 183.739648 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 57.40231 km |]
        , [u| 162.122172 km |]
        , [u| 183.739648 km |]
        ]

    , p7 mkPart
        [u| 57.40231 km |]
        [u| 104.719862 km |]
        [u| 21.617476 km |]

    , mkDay "Task 8" d8
        [u| 169.190764 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 57.473112 km |]
        , [u| 127.00634 km |]
        , [u| 169.190764 km |]
        ]

    , p8 mkPart
        [u| 57.473112 km |]
        [u| 69.533228 km |]
        [u| 42.184424 km |]
    ]
