{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flat.Forbes (unitsR) where

import Prelude hiding (span)
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
import Flat.Span (spanR)

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
mkDay = F.mkDayUnits (distancePointToPoint spanR)

mkPart
    :: TestName
    -> [Zone Rational]
    -> QTaskDistance Rational [u| m |]
    -> TestTree
mkPart = F.mkPartDayUnits (distancePointToPoint spanR)

unitsR :: TestTree
unitsR =
    testGroup "Forbes 2011/2012 distances"
    [ mkDay "Task 1" d1
        [u| 134.660084 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 54.601205 km |]
        , [u| 113.823497 km |]
        , [u| 134.660084 km |]
        ]

    , p1 mkPart
        [u| 54.601205km |]
        [u| 59.222292 km |]
        [u| 20.836587km |]

    , mkDay "Task 2" d2
        [u| 130.112778 km |]
        [ [u| 0 km |]
        , [u| 4.9 km |]
        , [u| 51.14458 km |]
        , [u| 91.782406 km |]
        , [u| 130.112778 km |]
        ]

    , p2 mkPart
        [u| 51.14458 km |]
        [u| 40.637825 km |]
        [u| 38.330373 km |]

    , mkDay "Task 3" d3
        [u| 185.317838 km |]
        [ [u| 0 km |]
        , [u| 24.9 km |]
        , [u| 77.973409 km |]
        , [u| 105.788963 km |]
        , [u| 185.3178383 km |]
        ]

    , p3 mkPart
        [u| 77.973409 km |]
        [u| 27.815554 km |]
        [u| 79.528874 km |]

    , mkDay "Task 4" d4
        [u| 157.11658 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 51.14458 km |]
        , [u| 157.11658 km |]
        ]

    , p4 mkPart
        [u| 51.14458km |]
        [u| 105.972002 km |]

    , mkDay "Task 5" d5
        [u| 221.37478 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 92.37285 km |]
        , [u| 221.37478 km |]
        ]

    , p5 mkPart
        [u| 92.37285 km |]
        [u| 129.00193 km |]

    , mkDay "Task 6" d6
        [u| 205.399824 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 130.286739 km |]
        , [u| 205.399824 km |]
        ]

    , p6 mkPart
        [u| 130.286739 km |]
        [u| 75.113085 km |]

    , mkDay "Task 7" d7
        [u| 183.555326 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 57.299009 km |]
        , [u| 161.981176 km |]
        , [u| 183.555326 km |]
        ]

    , p7 mkPart
        [u| 57.299009 km |]
        [u| 104.682168 km |]
        [u| 21.574149 km |]

    , mkDay "Task 8" d8
        [u| 168.85663 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 57.37677 km |]
        , [u| 126.72334 km |]
        , [u| 168.85663 km |]
        ]

    , p8 mkPart
        [u| 57.376771 km |]
        [u| 69.346572 km |]
        [u| 42.133287 km |]
    ]
