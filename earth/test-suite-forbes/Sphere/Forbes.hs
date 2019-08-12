{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sphere.Forbes (units) where

import Prelude hiding (span)
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
import ToLatLng (toLatLngD)
import Sphere.Span (spanD)

d1, d2, d3, d4, d5, d6, d7, d8 :: [Zone Double]
d1 = F.d1 toLatLngD
d2 = F.d2 toLatLngD
d3 = F.d3 toLatLngD
d4 = F.d4 toLatLngD
d5 = F.d5 toLatLngD
d6 = F.d6 toLatLngD
d7 = F.d7 toLatLngD
d8 = F.d8 toLatLngD

p1 = F.p1 toLatLngD
p2 = F.p2 toLatLngD
p3 = F.p3 toLatLngD
p4 = F.p4 toLatLngD
p5 = F.p5 toLatLngD
p6 = F.p6 toLatLngD
p7 = F.p7 toLatLngD
p8 = F.p8 toLatLngD

mkDay
    :: TestName
    -> [Zone Double]
    -> Quantity Double [u| km |]
    -> [Quantity Double [u| km |]]
    -> TestTree
mkDay = F.mkDayUnits (distancePointToPoint spanD)

mkPart
    :: TestName
    -> [Zone Double]
    -> QTaskDistance Double [u| m |]
    -> TestTree
mkPart = F.mkPartDayUnits (distancePointToPoint spanD)

units :: TestTree
units =
    testGroup "Forbes 2011/2012 distances"
    [ mkDay "Task 1" d1
        [u| 134.917675 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 54.755578 km |]
        , [u| 114.032205 km |]
        , [u| 134.917675 km |]
        ]

    , p1 mkPart
        [u| 54.755579 km |]
        [u| 59.276628 km |]
        [u| 20.88547 km |]

    , mkDay "Task 2" d2
        [u| 130.167733 km |]
        [ [u| 0 km |]
        , [u| 4.9 km |]
        , [u| 51.290669 km |]
        , [u| 91.860213 km |]
        , [u| 130.167733 km |]
        ]

    , p2 mkPart
        [u| 51.290669 km |]
        [u| 40.569545 km |]
        [u| 38.30752 km |]

    , mkDay "Task 3" d3
        [u| 185.643415 km |]
        [ [u| 0 km |]
        , [u| 24.9 km |]
        , [u| 78.147093 km |]
        , [u| 105.927192 km |]
        , [u| 185.643415 km |]
        ]

    , p3 mkPart
        [u| 78.147093 km |]
        [u| 27.780099 km |]
        [u| 79.716223 km |]

    , mkDay "Task 4" d4
        [u| 157.163219 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 51.290669 km |]
        , [u| 157.163219 km |]
        ]

    , p4 mkPart
        [u| 51.290669 km |]
        [u| 105.872549 km |]

    , mkDay "Task 5" d5
        [u| 221.477524 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 92.601903 km |]
        , [u| 221.477524 km |]
        ]

    , p5 mkPart
        [u| 92.601903 km |]
        [u| 128.875621 km |]

    , mkDay "Task 6" d6
        [u| 205.844958 km |]
        [ [u| 0 km |]
        , [u| 14.9 km |]
        , [u| 130.665489 km |]
        , [u| 205.844958 km |]
        ]

    , p6 mkPart
        [u| 130.665489 km |]
        [u| 75.179469 km |]

    , mkDay "Task 7" d7
        [u| 183.48893 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 57.365312 km |]
        , [u| 161.875045 km |]
        , [u| 183.48893 km |]
        ]

    , p7 mkPart
        [u| 57.365312 km |]
        [u| 104.509733 km |]
        [u| 21.613885 km |]

    , mkDay "Task 8" d8
        [u| 169.10714 km |]
        [ [u| 0 km |]
        , [u| 9.9 km |]
        , [u| 57.42751 km |]
        , [u| 126.975179 km |]
        , [u| 169.10714 km |]
        ]

    , p8 mkPart
        [u| 57.42751 km |]
        [u| 69.547668 km |]
        [u| 42.131962 km |]
    ]
