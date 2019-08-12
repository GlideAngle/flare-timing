{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Ellipsoid.AndoyerLambert.Forbes (unitsR) where

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
import Ellipsoid.AndoyerLambert.Span (spanR)

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
        [u| 134.578761 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 54.546328 km |]
        , [u| 113.761542 km |]
        , [u| 134.578761 km |]
        ]

    , p1 mkPart
        [u| 54.546328 km |]
        [u| 59.215214 km |]
        [u| 20.817219 km |]

    , mkDay "Task 2" d2
        [u| 130.139874 km |]
        [ [u| 0.0 km |]
        , [u| 4.9 km |]
        , [u| 51.091366 km |]
        , [u| 91.780972 km |]
        , [u| 130.139874 km |]
        ]

    , p2 mkPart
        [u| 51.091366 km |]
        [u| 40.689605 km |]
        [u| 38.358902 km |]

    , mkDay "Task 3" d3
        [u| 185.205354 km |]
        [ [u| 0.0 km |]
        , [u| 24.9 km |]
        , [u| 77.915042 km |]
        , [u| 105.75581 km |]
        , [u| 185.205354 km |]
        ]

    , p3 mkPart
        [u| 77.915042 km |]
        [u| 27.840769 km |]
        [u| 79.449544 km |]

    , mkDay "Task 4" d4
        [u| 157.129099 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 51.091366 km |]
        , [u| 157.129099 km |]
        ]

    , p4 mkPart
        [u| 51.091366 km |]
        [u| 106.037732 km |]

    , mkDay "Task 5" d5
        [u| 221.347704 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 92.289868 km |]
        , [u| 221.347704 km |]
        ]

    , p5 mkPart
        [u| 92.289868 km |]
        [u| 129.057836 km |]

    , mkDay "Task 6" d6
        [u| 205.255073 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 130.150746 km |]
        , [u| 205.255073 km |]
        ]

    , p6 mkPart
        [u| 130.150746 km |]
        [u| 75.104328 km |]

    , mkDay "Task 7" d7
        [u| 183.648575 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 57.291408 km |]
        , [u| 162.092998 km |]
        , [u| 183.648575 km |]
        ]

    , p7 mkPart
        [u| 57.291408 km |]
        [u| 104.80159 km |]
        [u| 21.555577 km |]

    , mkDay "Task 8" d8
        [u| 168.810956 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 57.377796 km |]
        , [u| 126.658175 km |]
        , [u| 168.810956 km |]
        ]

    , p8 mkPart
        [u| 57.377796 km |]
        [u| 69.280379 km |]
        [u| 42.152781 km |]
    ]
