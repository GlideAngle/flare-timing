{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Ellipsoid.Vincenty.Forbes (unitsR) where

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
import Ellipsoid.Vincenty.Span (spanR)

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

unitsR :: TestTree
unitsR =
    testGroup "Forbes 2011/2012 distances"
    [ mkDay "Task 1" d1
        [u| 134.69636 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 54.61796 km |]
        , [u| 113.85564 km |]
        , [u| 134.69636 km |]
        ]

    , p1 mkPart
        [u| 54.617964 km |]
        [u| 59.237679 km |]
        [u| 20.840718 km |]

    , mkDay "Task 2" d2
        [u| 130.15478 km |]
        [ [u| 0 km |]
        , [u| 4.9 km |]
        , [u| 51.16 km |]
        , [u| 91.81142 km |]
        , [u| 130.15478 km |]
        ]

    , p2 mkPart
        [u| 51.16 km |]
        [u| 40.651422km |]
        [u| 38.343456 km |]

    , mkDay "Task 3" d3
         [u| 185.356167 km |]
        [ [u| 0 km |]
        , [u| 24.9 km |]
        , [u| 77.994969 km |]
        , [u| 105.816215 km |]
        , [u| 185.356167 km |]
        ]

    , p3 mkPart
        [u| 77.994969 km |]
        [u| 27.821246 km |]
        [u| 79.539952 km |]

    , mkDay "Task 4" d4
        [u| 157.147977 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 51.16 km |]
        , [u| 157.147977 km |]
        ]

    , p4 mkPart
        [u| 51.16 km |]
        [u| 105.987977 km |]

    , mkDay "Task 5" d5
        [u| 221.402575 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 92.398127 km |]
        , [u| 221.402575 km |]
        ]

    , p5 mkPart
        [u| 92.398127 km |]
        [u| 129.004449 km |]

    , mkDay "Task 6" d6
        [u| 205.462898 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 130.32874 km |]
        , [u| 205.462898 km |]
        ]

    , p6 mkPart
        [u| 130.328741 km |]
        [u| 75.134157 km |]

    , mkDay "Task 7" d7
        [u| 183.60369 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 57.31847 km |]
        , [u| 162.02765 km |]
        , [u| 183.60369 km |]
        ]

    , p7 mkPart
        [u| 57.318473 km |]
        [u| 104.709173 km |]
        [u| 21.576049 km |]

    , mkDay "Task 8" d8
        [u| 168.916012 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 57.396389 km |]
        , [u| 126.768465 km |]
        , [u| 168.916012 km |]
        ]

    , p8 mkPart
        [u| 57.396389 km |]
        [u| 69.372076 km |]
        [u| 42.147547 km |]
    ]
