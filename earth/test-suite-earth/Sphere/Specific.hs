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

module Sphere.Specific (specificUnits) where

import Prelude hiding (span)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.List (inits)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit as HU ((@?=), (@?), testCase)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Bifunctor.Flip (Flip(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng, fromKms)
import Flight.Zone (Bearing(..), Radius(..), Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.Cylinder
    (Samples(..), SampleParams(..), Tolerance(..), CircumSample, ZonePoint(..))
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Rational as Rat (circumSample)
import Data.Number.RoundingFunctions (dpRound)
import qualified Forbes as F (d1, d2, d3, d4, d5, d6, d7, d8)
import qualified Sphere.Forbes as F
    ( dd1, dd2, dd3, dd4, dd5, dd6, dd7, dd8
    , dsd1, dsd2, dsd3, dsd4, dsd5, dsd6, dsd7, dsd8
    )
import Forbes (toLL)

(.>=.) :: (Show a, Show b) => a -> b -> String
(.>=.) x y = show x ++ " >= " ++ show y

(.~=.) :: (Show a, Show b) => a -> b -> String
(.~=.) x y = show x ++ " ~= " ++ show y

specificUnits :: TestTree
specificUnits =
    testGroup "Zone edge shortest path unit tests"
    [ circumSampleUnits
    , forbesUnits
    ]

m100 :: Tolerance Rational
m100 = Tolerance $ 100 % 1

mm100 :: Tolerance Rational
mm100 = Tolerance $ 100 % 1000

mm30 :: Tolerance Rational
mm30 = Tolerance $ 30 % 1000

mm10 :: Tolerance Rational
mm10 = Tolerance $ 10 % 1000

mm1 :: Tolerance Rational
mm1 = Tolerance $ 1 % 1000

sampleParams :: SampleParams Rational
sampleParams = SampleParams { spSamples = Samples 100
                            , spTolerance = mm30
                            }

ll :: LatLng Rational [u| rad |]
ll =
    LatLng (lat, lng)
    where
        oneRadian = [u| 1 rad |]
        lat = Lat oneRadian
        lng = Lng oneRadian

br :: Bearing Rational
br = let (Epsilon e) = defEps in (Bearing . MkQuantity $ F.pi e)

circumSampleUnits :: TestTree
circumSampleUnits =
    testGroup "Points just within the zone"
    [ testGroup "Outside the zone."
        [ HU.testCase
            "No points > 0mm outside a 40m cylinder when searching within 1mm" $
            zpFilter (>) ll ([u| 40 m |])
            (fst $ cs (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ [u| 40 m |]) ll))
            @?= [] 

        , HU.testCase
            "No points > 0mm outside a 400m cylinder when searching within 1mm" $
            zpFilter (>) ll ([u| 400 m |])
            (fst $ cs (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ [u| 400 m |]) ll))
            @?= [] 

        , HU.testCase
            "No points > 0mm outside a 1km cylinder when searching within 10mm" $
            zpFilter (>) ll (convert [u| 1 km |]) 
            (fst $ cs (sampleParams { spTolerance = mm10 }) br Nothing (Cylinder (Radius $ convert [u| 1 km |]) ll))
            @?= [] 

        , HU.testCase
            "No points > 0mm outside a 10km cylinder when searching within 100mm" $
            zpFilter (>) ll (convert [u| 10 km |])
            (fst $ cs (sampleParams { spTolerance = mm100 }) br Nothing (Cylinder (Radius $ convert [u| 10 km |]) ll))
            @?= [] 

        , HU.testCase 
            "No points > 0m outside a 100km cylinder when searching within 100m" $
            zpFilter (>) ll (convert [u| 100 km |])
            (fst $ cs (sampleParams { spTolerance = m100 }) br Nothing (Cylinder (Radius $ convert [u| 100 km |]) ll))
            @?= [] 
        ]
    , testGroup "Inside the zone."
        [ HU.testCase
            "No points > 1mm inside a 40m cylinder when searching within 1mm" $
            zpFilter (<) ll ([u| 40 m |] -: convert [u| 1 mm |])
            (fst $ cs (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ [u| 40 m |]) ll))
            @?= [] 

        , HU.testCase
            "No points > 1mm inside a 400m cylinder when searching within 1mm" $
            zpFilter (<) ll ([u| 400 m |] -: convert [u| 1 mm |])
            (fst $ cs (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ [u| 400 m |]) ll))
            @?= [] 

        , HU.testCase
            "No points > 9mm inside a 1km cylinder when searching within 10mm" $
            zpFilter (<) ll (convert [u| 1 km |] -: convert [u| 9 mm |])
            (fst $ cs (sampleParams { spTolerance = mm10 }) br Nothing (Cylinder (Radius $ convert [u| 1 km |]) ll))
            @?= [] 

        , HU.testCase
            "No points > 97mm inside a 10km cylinder when searching within 100mm" $
            zpFilter (<) ll (convert [u| 10 km |] -: convert [u| 97 mm |])
            (fst $ cs (sampleParams { spTolerance = mm100 }) br Nothing (Cylinder (Radius $ convert [u| 10 km |]) ll))
            @?= [] 

        , HU.testCase
            "No points > 85m inside a 100km cylinder when searching within 100m" $
            zpFilter (<) ll (convert [u| 100 km |] -: [u| 85 m |])
            (fst $ cs (sampleParams { spTolerance = m100 }) br Nothing (Cylinder (Radius $ convert [u| 100 km |] ) ll))
            @?= [] 
        ]
    ]

zpFilter
    :: (Quantity Rational [u| m |] -> Quantity Rational [u| m |] -> Bool)
    -> LatLng Rational [u| rad |]
    -> Quantity Rational [u| m |]
    -> [ZonePoint Rational]
    -> [ZonePoint Rational]
zpFilter cmp origin d =
    filter (\x -> zpDistance origin x `cmp` d)

zpDistance
    :: LatLng Rational [u| rad |]
    -> ZonePoint Rational
    -> Quantity Rational [u| m |]
zpDistance origin ZonePoint{point} =
    d
    where
        TaskDistance d =
            edgesSum $ distancePointToPoint span [Point origin, Point point]

forbesUnits :: TestTree
forbesUnits =
    testGroup "Forbes 2011/2012 distances"
    [ day1PartUnits
    , day1Units

    , day2PartUnits
    , day2Units

    , day3PartUnits
    , day3Units

    , day4PartUnits
    , day4Units

    , day5PartUnits
    , day5Units

    , day6PartUnits
    , day6Units

    , day7PartUnits
    , day7Units

    , day8PartUnits
    , day8Units
    ]

mkPartDayUnits :: TestName
               -> [Zone Rational]
               -> TaskDistance Rational
               -> TestTree
mkPartDayUnits title zs (TaskDistance d) = testGroup title
    [ HU.testCase
        ( "point-to-point distance "
        ++ show td'
        ++ " ~= "
        ++ show tdR
        )
        $ (tdR' == tdR) @? tdR' .~=. tdR
    ]
    where
        dKm = convert d :: Quantity Rational [u| km |]
        Flip r = dpRound 3 <$> Flip dKm
        tdR = TaskDistance (convert r :: Quantity Rational [u| m |])

        td'@(TaskDistance d') = edgesSum $ distancePointToPoint span zs
        dKm' = convert d' :: Quantity Rational [u| km |]
        Flip r' = dpRound 3 <$> Flip dKm'
        tdR' = TaskDistance (convert r' :: Quantity Rational [u| m |])

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

mkDayUnits :: TestName
           -> [Zone Rational]
           -> TaskDistance Rational
           -> [TaskDistance Rational]
           -> TestTree
mkDayUnits title pDay dDay' dsDay' = testGroup title
    [ HU.testCase
        ("point-to-point distance >= " ++ show dDay)
        $ (ppDay >= dDay) @? ppDay .>=. dDay

    , testGroup
        ( "\n>=\n"
        ++ show ppDayInits
        ++ "\n"
        ++ show dsDay
        )
        [ HU.testCase "point-to-point distances"
          $ (ppDayInits >= dsDay) @? ppDayInits .>=. dsDay
        ]
    ]
    where
        dDay = tdRound dDay'
        dsDay = tdRound <$> dsDay'

        pp :: [Zone Rational] -> PathDistance Rational
        pp = distancePointToPoint span

        ppDay :: TaskDistance Rational
        ppDay = tdRound . edgesSum $ pp pDay

        pDayInits :: [[Zone Rational]]
        pDayInits = drop 1 $ inits pDay

        ppDayInits :: [TaskDistance Rational]
        ppDayInits = tdRound . edgesSum . pp <$> pDayInits

tdRound :: TaskDistance Rational -> TaskDistance Rational
tdRound (TaskDistance (MkQuantity d)) =
    TaskDistance . MkQuantity . dpRound 2 $ d

day1Units :: TestTree
day1Units = mkDayUnits "Task 1" F.d1 F.dd1 F.dsd1

day2Units :: TestTree
day2Units = mkDayUnits "Task 2" F.d2 F.dd2 F.dsd2

day3Units :: TestTree
day3Units = mkDayUnits "Task 3" F.d3 F.dd3 F.dsd3

day4Units :: TestTree
day4Units = mkDayUnits "Task 4" F.d4 F.dd4 F.dsd4

day5Units :: TestTree
day5Units = mkDayUnits "Task 5" F.d5 F.dd5 F.dsd5

day6Units :: TestTree
day6Units = mkDayUnits "Task 6" F.d6 F.dd6 F.dsd6

day7Units :: TestTree
day7Units = mkDayUnits "Task 7" F.d7 F.dd7 F.dsd7

day8Units :: TestTree
day8Units = mkDayUnits "Task 8" F.d8 F.dd8 F.dsd8

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

cs :: CircumSample Rational
cs = Rat.circumSample
