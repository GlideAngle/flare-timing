{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Ellipsoid.ForsytheAndoyerLambert.Forbes (unitsR) where

import Test.Tasty (TestTree, TestName, testGroup)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Distance (QTaskDistance)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import qualified Forbes as F
    ( mkDayUnits, mkPartDayUnits, toLatLngR
    , d1, d2, d3, d4, d5, d6, d7, d8
    , p1, p2, p3, p4, p5, p6, p7, p8
    )
import Flight.Earth.Ellipsoid (wgs84)
import Ellipsoid.ForsytheAndoyerLambert.Span (spanR)

d1, d2, d3, d4, d5, d6, d7, d8 :: [Zone Rational]
d1 = F.d1 F.toLatLngR
d2 = F.d2 F.toLatLngR
d3 = F.d3 F.toLatLngR
d4 = F.d4 F.toLatLngR
d5 = F.d5 F.toLatLngR
d6 = F.d6 F.toLatLngR
d7 = F.d7 F.toLatLngR
d8 = F.d8 F.toLatLngR

p1 = F.p1 F.toLatLngR
p2 = F.p2 F.toLatLngR
p3 = F.p3 F.toLatLngR
p4 = F.p4 F.toLatLngR
p5 = F.p5 F.toLatLngR
p6 = F.p6 F.toLatLngR
p7 = F.p7 F.toLatLngR
p8 = F.p8 F.toLatLngR

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
        [u| 134.578203 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 54.545966 km |]
        , [u| 113.761093 km |]
        , [u| 134.578203 km |]
        ]

    , p1 mkPart
        [u| 54.545966 km |]
        [u| 59.215127 km |]
        [u| 20.81711 km |]

    , mkDay "Task 2" d2
        [u| 130.139565 km |]
        [ [u| 0.0 km |]
        , [u| 4.9 km |]
        , [u| 51.091048 km |]
        , [u| 91.780649 km |]
        , [u| 130.139565 km |]
        ]

    , p2 mkPart
        [u| 51.091048 km |]
        [u| 40.689601 km |]
        [u| 38.358917 km |]

    , mkDay "Task 3" d3
        [u| 185.204474 km |]
        [ [u| 0.0 km |]
        , [u| 24.9 km |]
        , [u| 77.91467 km |]
        , [u| 105.755445 km |]
        , [u| 185.204474 km |]
        ]

    , p3 mkPart
        [u| 77.91467 km |]
        [u| 27.840775 km |]
        [u| 79.449029 km |]

    , mkDay "Task 4" d4
        [u| 157.128825 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 51.091048 km |]
        , [u| 157.128825 km |]
        ]

    , p4 mkPart
        [u| 51.091048 km |]
        [u| 106.037777 km |]

    , mkDay "Task 5" d5
        [u| 221.34732 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 92.28942 km |]
        , [u| 221.34732 km |]
        ]

    , p5 mkPart
        [u| 92.28942 km |]
        [u| 129.0579 km |]

    , mkDay "Task 6" d6
        [u| 205.254273 km |]
        [ [u| 0.0 km |]
        , [u| 14.9 km |]
        , [u| 130.149972 km |]
        , [u| 205.254273 km |]
        ]

    , p6 mkPart
        [u| 130.149972 km |]
        [u| 75.104301 km |]

    , mkDay "Task 7" d7
        [u| 183.648418 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 57.291325 km |]
        , [u| 162.092915 km |]
        , [u| 183.648418 km |]
        ]

    , p7 mkPart
        [u| 57.291325 km |]
        [u| 104.80159 km |]
        [u| 21.555504 km |]

    , mkDay "Task 8" d8
        [u| 168.81044 km |]
        [ [u| 0.0 km |]
        , [u| 9.9 km |]
        , [u| 57.377727 km |]
        , [u| 126.657655 km |]
        , [u| 168.81044 km |]
        ]

    , p8 mkPart
        [u| 57.377727 km |]
        [u| 69.279929 km |]
        [u| 42.152785 km |]
    ]
