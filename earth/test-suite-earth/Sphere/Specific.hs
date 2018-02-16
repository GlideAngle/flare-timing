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

module Sphere.Specific (forbesUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, TestName, testGroup)
import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), SpanLatLng, fromKms)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Forbes as F
    ( d1, d2, d3, d4, d5, d6, d7, d8
    , mkDayUnits, mkPartDayUnits
    )
import qualified Sphere.Forbes as F
    ( dd1, dd2, dd3, dd4, dd5, dd6, dd7, dd8
    , dsd1, dsd2, dsd3, dsd4, dsd5, dsd6, dsd7, dsd8
    )
import Forbes (toLL)

mkDayUnits
    :: TestName
    -> [Zone Rational]
    -> TaskDistance Rational
    -> [TaskDistance Rational]
    -> TestTree
mkDayUnits = F.mkDayUnits (distancePointToPoint span)

mkPartDayUnits
    :: TestName
    -> [Zone Rational]
    -> TaskDistance Rational
    -> TestTree
mkPartDayUnits = F.mkPartDayUnits (distancePointToPoint span)

forbesUnits :: TestTree
forbesUnits =
    testGroup "Forbes 2011/2012 distances"
    [ day1PartUnits
    , mkDayUnits "Task 1" F.d1 F.dd1 F.dsd1

    , day2PartUnits
    , mkDayUnits "Task 2" F.d2 F.dd2 F.dsd2

    , day3PartUnits
    , mkDayUnits "Task 3" F.d3 F.dd3 F.dsd3

    , day4PartUnits
    , mkDayUnits "Task 4" F.d4 F.dd4 F.dsd4

    , day5PartUnits
    , mkDayUnits "Task 5" F.d5 F.dd5 F.dsd5

    , day6PartUnits
    , mkDayUnits "Task 6" F.d6 F.dd6 F.dsd6

    , day7PartUnits
    , mkDayUnits "Task 7" F.d7 F.dd7 F.dsd7

    , day8PartUnits
    , mkDayUnits "Task 8" F.d8 F.dd8 F.dsd8
    ]

day1PartUnits :: TestTree
day1PartUnits =
    testGroup "Task 1 [...]"
    [ mkPartDayUnits "Task 1 [x, x, _, _]" p1 d1
    , mkPartDayUnits "Task 1 [_, x, x, _]" p2 d2
    , mkPartDayUnits "Task 1 [_, _, x, x]" p3 d3
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 33.85373, 147.94195)
                , (negate 33.4397, 148.34533)
                , (negate 33.61965, 148.4099)
                ]

            p1 = take 2 xs
            d1 = fromKms [u| 54.755578 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 59.276627 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 20.88547 km |]

day2PartUnits :: TestTree
day2PartUnits =
    testGroup "Task 2 [...]"
    [ mkPartDayUnits "Task 2 [x, x, _, _]" p1 d1
    , mkPartDayUnits "Task 2 [_, x, x, _]" p2 d2
    , mkPartDayUnits "Task 2 [_, _, x, x]" p3 d3
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 32.90223, 147.98492)
                , (negate 32.9536, 147.55457)
                , (negate 33.12592, 147.91043)
                ]

            p1 = take 2 xs
            d1 = fromKms [u| 51.290669 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 40.569544 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 38.30752 km |]

day3PartUnits :: TestTree
day3PartUnits =
    testGroup "Task 3 [...]"
    [ mkPartDayUnits "Task 3 [x, x, _, _]" p1 d1
    , mkPartDayUnits "Task 3 [_, x, x, _]" p2 d2
    , mkPartDayUnits "Task 3 [_, _, x, x]" p3 d3
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 34.02107, 148.2233)
                , (negate 34.11795, 148.5013)
                , (negate 34.82197, 148.66543)
                ]

            p1 = take 2 xs
            d1 = fromKms [u| 78.147093 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 27.780099 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 79.716223 km |]

day4PartUnits :: TestTree
day4PartUnits =
    testGroup "Task 4 [...]"
    [ mkPartDayUnits "Task 4 [x, x, _]" p1' d1
    , mkPartDayUnits "Task 4 [_, x, x]" p2 d2
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 32.90223, 147.98492)
                , (negate 32.46363, 148.989)
                ]

            -- NOTE: Use p1' to avoid an hlint duplication warning.
            p1' = take 2 xs
            d1 = fromKms [u| 51.290669 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 105.87255 km |]

day5PartUnits :: TestTree
day5PartUnits =
    testGroup "Task 5 [...]"
    [ mkPartDayUnits "Task 5 [x, x, _]" p1 d1
    , mkPartDayUnits "Task 5 [_, x, x]" p2 d2
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 32.56608, 148.22657)
                , (negate 32.0164, 149.43363)
                ]

            p1 = take 2 xs
            d1 = fromKms [u| 92.601904 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 128.87562 km |]

day6PartUnits :: TestTree
day6PartUnits =
    testGroup "Task 6 [...]"
    [ mkPartDayUnits "Task 6 [x, x, _]" p1 d1
    , mkPartDayUnits "Task 6 [_, x, x]" p2 d2
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 32.19498, 147.76218)
                , (negate 31.69323, 148.29623)
                ]

            p1 = take 2 xs
            d1 = fromKms [u| 130.665489 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 75.17947 km |]

day7PartUnits :: TestTree
day7PartUnits =
    testGroup "Task 7 [...]"
    [ mkPartDayUnits "Task 7 [x, x, _, _]" p1 d1
    , mkPartDayUnits "Task 7 [_, x, x, _]" p2 d2
    , mkPartDayUnits "Task 7 [_, _, x, x]" p3 d3
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 32.9536, 147.55457)
                , (negate 32.76052, 148.64958)
                , (negate 32.93585, 148.74947)
                ]

            p1 = take 2 xs
            d1 = fromKms [u| 57.365312 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 104.509732 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 21.613886 km |]


day8PartUnits :: TestTree
day8PartUnits =
    testGroup "Task 8 [...]"
    [ mkPartDayUnits "Task 8 [x, x, _, _]" p1 d1
    , mkPartDayUnits "Task 8 [_, x, x, _]" p2 d2
    , mkPartDayUnits "Task 8 [_, _, x, x]" p3 d3
    ]
        where
            xs =
                Point . toLL <$>
                [ (negate 33.36137, 147.93207)
                , (negate 33.75343, 147.52865)
                , (negate 33.12908, 147.57323)
                , (negate 33.361, 147.9315)
                ]

            p1 = take 2 xs
            d1 = fromKms [u| 57.427511 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 69.547668 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 42.131961 km |]

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps
