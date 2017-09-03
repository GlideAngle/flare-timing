{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module EdgeToEdge (edgeToEdgeUnits) where

import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.List (inits)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit as HU ((@?=), (@?), testCase)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Number.RoundingFunctions (dpRound)
import Data.Bifunctor.Flip (Flip(..))

import qualified Flight.Task as FS
import Flight.Task
    ( LatLng(..)
    , Bearing(..)
    , Radius(..)
    , Samples(..)
    , SampleParams(..)
    , Tolerance(..)
    , Zone(..)
    , TaskDistance(..)
    , EdgeDistance(..)
    , DistancePath(..)
    , fromKms
    )
import Flight.Geo (Epsilon(..), Lat(..), Lng(..), defEps)
import Flight.Units ()

(.>=.) :: (Show a, Show b) => a -> b -> String
(.>=.) x y = show x ++ " >= " ++ show y

(.<=.) :: (Show a, Show b) => a -> b -> String
(.<=.) x y = show x ++ " <= " ++ show y

(.~=.) :: (Show a, Show b) => a -> b -> String
(.~=.) x y = show x ++ " ~= " ++ show y

edgeToEdgeUnits :: TestTree
edgeToEdgeUnits = testGroup "Zone edge shortest path unit tests"
    [ circumSampleUnits
    , forbesUnits
    ]

m100 :: Tolerance
m100 = Tolerance $ 100 % 1

mm100 :: Tolerance
mm100 = Tolerance $ 100 % 1000

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

mm10 :: Tolerance
mm10 = Tolerance $ 10 % 1000

mm1 :: Tolerance
mm1 = Tolerance $ 1 % 1000

sampleParams :: SampleParams
sampleParams = SampleParams { spSamples = Samples 100
                            , spTolerance = mm30
                            }

ll :: LatLng [u| rad |]
ll =
    LatLng (lat, lng)
    where
        oneRadian = [u| 1 rad |]
        lat = Lat oneRadian
        lng = Lng oneRadian

br :: Bearing
br = let (Epsilon e) = defEps in (Bearing . MkQuantity $ F.pi e)

circumSampleUnits :: TestTree
circumSampleUnits = testGroup "Points just within the zone"
    [ testGroup "Outside the zone."
        [ HU.testCase
            "No points > 0mm outside a 40m cylinder when searching within 1mm" $
            filter (> 40 + unmilli 0)
            (snd $ FS.circumSample (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ MkQuantity 40) ll))
            @?= [] 

        , HU.testCase
            "No points > 0mm outside a 400m cylinder when searching within 1mm" $
            filter (> 400 + unmilli 0)
            (snd $ FS.circumSample (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ MkQuantity 400) ll))
            @?= [] 

        , HU.testCase
            "No points > 27mm outside a 1km cylinder when searching within 10mm" $
            filter (> unkilo 1 + unmilli 27) 
            (snd $ FS.circumSample (sampleParams { spTolerance = mm10 }) br Nothing (Cylinder (Radius $ MkQuantity $ unkilo 1) ll))
            @?= [] 

        , HU.testCase
            "No points > 80mm outside a 10km cylinder when searching within 100mm" $
            filter (> unkilo 10 + unmilli 80)
            (snd $ FS.circumSample (sampleParams { spTolerance = mm100 }) br Nothing (Cylinder (Radius $ MkQuantity $ unkilo 10) ll))
            @?= [] 

        , HU.testCase 
            "No points > 80m outside a 100km cylinder when searching within 100m" $
            filter (> unkilo 100 + 80)
            (snd $ FS.circumSample (sampleParams { spTolerance = m100 }) br Nothing (Cylinder (Radius $ MkQuantity $ unkilo 100) ll))
            @?= [] 
        ]
    , testGroup "Inside the zone."
        [ HU.testCase
            "No points > 1mm inside a 40m cylinder when searching within 1mm" $
            filter (< 40.0 - unmilli 1)
            (snd $ FS.circumSample (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ MkQuantity 40) ll))
            @?= [] 

        , HU.testCase
            "No points > 1mm inside a 400m cylinder when searching within 1mm" $
            filter (< 400.0 - unmilli 1)
            (snd $ FS.circumSample (sampleParams { spTolerance = mm1 }) br Nothing (Cylinder (Radius $ MkQuantity 400) ll))
            @?= [] 

        , HU.testCase
            "No points > 50mm inside a 1km cylinder when searching within 10mm" $
            filter (< unkilo 1 - unmilli 50)
            (snd $ FS.circumSample (sampleParams { spTolerance = mm10 }) br Nothing (Cylinder (Radius $ MkQuantity $ unkilo 1) ll))
            @?= [] 

        , HU.testCase
            "No points > 80mm inside a 10km cylinder when searching within 100mm" $
            filter (< unkilo 10 - unmilli 100)
            (snd $ FS.circumSample (sampleParams { spTolerance = mm100 }) br Nothing (Cylinder (Radius $ MkQuantity $ unkilo 10) ll))
            @?= [] 

        , HU.testCase
            "No points > 80m inside a 100km cylinder when searching within 100m" $
            filter (< unkilo 100 - 80)
            (snd $ FS.circumSample (sampleParams { spTolerance = m100 }) br Nothing (Cylinder (Radius $ MkQuantity $ unkilo 100) ll))
            @?= [] 
        ]
    ]

-- | The input pair is in degrees while the output is in radians.
toLL :: (Double, Double) -> LatLng [u| rad |]
toLL (lat, lng) =
    LatLng (Lat lat'', Lng lng'')
        where
            lat' = (MkQuantity $ toRational lat) :: Quantity Rational [u| deg |]
            lng' = (MkQuantity $ toRational lng) :: Quantity Rational [u| deg |]
            lat'' = convert lat' :: Quantity Rational [u| rad |]
            lng'' = convert lng' :: Quantity Rational [u| rad |]

forbesUnits :: TestTree
forbesUnits = testGroup "Forbes 2011/2012 distances"
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

unmilli :: Fractional a => a -> a
unmilli x = x / 1000

unkilo :: Num a => a -> a
unkilo x = x * 1000

mkPartDayUnits :: TestName
               -> [Zone]
               -> TaskDistance
               -> TestTree
mkPartDayUnits title zs (TaskDistance d) = testGroup title
    [ HU.testCase
        ("point-to-point distance " ++ show td' ++ " ~= " ++ show tdR)
        $ (tdR' == tdR) @? tdR' .~=. tdR
    ]
    where
        dKm = convert d :: Quantity Rational [u| km |]
        Flip r = dpRound 2 <$> Flip dKm
        tdR = TaskDistance (convert r :: Quantity Rational [u| m |])

        td'@(TaskDistance d') = FS.distancePointToPoint zs
        dKm' = convert d' :: Quantity Rational [u| km |]
        Flip r' = dpRound 2 <$> Flip dKm'
        tdR' = TaskDistance (convert r' :: Quantity Rational [u| m |])

day1PartUnits :: TestTree
day1PartUnits = testGroup "Task 1 [...]"
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
            d1 = fromKms [u| 54.76 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 59.28 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 20.89 km |]

day2PartUnits :: TestTree
day2PartUnits = testGroup "Task 2 [...]"
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
            d1 = fromKms [u| 51.29 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 40.57 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 38.31 km |]

day3PartUnits :: TestTree
day3PartUnits = testGroup "Task 3 [...]"
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
            d1 = fromKms [u| 78.15 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 27.78 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 79.72 km |]

day4PartUnits :: TestTree
day4PartUnits = testGroup "Task 4 [...]"
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
            d1 = fromKms [u| 51.29 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 105.9 km |]

day5PartUnits :: TestTree
day5PartUnits = testGroup "Task 5 [...]"
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
            d1 = fromKms [u| 92.6 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 128.9 km |]

day6PartUnits :: TestTree
day6PartUnits = testGroup "Task 6 [...]"
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
            d1 = fromKms [u| 130.7 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 75.18 km |]

day7PartUnits :: TestTree
day7PartUnits = testGroup "Task 7 [...]"
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
            d1 = fromKms [u| 57.37 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 104.5 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 21.61 km |]


day8PartUnits :: TestTree
day8PartUnits = testGroup "Task 8 [...]"
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
            d1 = fromKms [u| 57.43 km |]

            p2 = take 2 $ drop 1 xs
            d2 = fromKms [u| 69.55 km |]

            p3 = take 2 $ drop 2 xs
            d3 = fromKms [u| 42.13 km |]

mkDayUnits :: TestName
           -> [Zone]
           -> TaskDistance
           -> [TaskDistance]
           -> TestTree
mkDayUnits title pDay dDay dsDay = testGroup title
    [ HU.testCase "zones are separated" $ FS.separatedZones pDay @?= True

    , HU.testCase
        ("point-to-point distance >= " ++ show dDay)
        $ (ppDay >= dDay) @? ppDay .>=. dDay

    , HU.testCase
        ("edge-to-edge distance <= " ++ show dDay)
        $ (eeDay <= dDay) @? eeDay .<=. dDay

    , HU.testCase
        ("point-to-point distances "
        ++ show ppDayInits
        ++ " >= "
        ++ show dsDay
        ) $
        (ppDayInits >= dsDay) @? ppDayInits .>=. dsDay

    , HU.testCase
        ("edge-to-edge distances "
        ++ show eeDayInits
        ++ " <= "
        ++ show dsDay
        ) $
            distLess eeDayInits dsDay @? eeDayInits .<=. dsDay
    ]
    where
        pp = FS.distancePointToPoint
        ee = FS.distanceEdgeToEdge PathPointToZone mm30 
        ppDay = pp pDay
        eeDay = centers $ ee pDay
        pDayInits = drop 1 $ inits pDay
        ppDayInits = pp <$> pDayInits
        eeDayInits = centers . ee <$> pDayInits
        distLess xs ys = take 1 (reverse xs) < take 1 (reverse ys)

day1Units :: TestTree
day1Units = mkDayUnits "Task 1" pDay1 dDay1 dsDay1

day2Units :: TestTree
day2Units = mkDayUnits "Task 2" pDay2 dDay2 dsDay2

day3Units :: TestTree
day3Units = mkDayUnits "Task 3" pDay3 dDay3 dsDay3

day4Units :: TestTree
day4Units = mkDayUnits "Task 4" pDay4 dDay4 dsDay4

day5Units :: TestTree
day5Units = mkDayUnits "Task 5" pDay5 dDay5 dsDay5

day6Units :: TestTree
day6Units = mkDayUnits "Task 6" pDay6 dDay6 dsDay6

day7Units :: TestTree
day7Units = mkDayUnits "Task 7" pDay7 dDay7 dsDay7

day8Units :: TestTree
day8Units = mkDayUnits "Task 8" pDay8 dDay8 dsDay8

{-
SEE: http://www.stevemorse.org/nearest/distancebatch.html
SEE: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
SEE: http://www.anycalculator.com/longitude.htm
SEE: http://andrew.hedges.name/experiments/haversine/

-33.36137, 147.93207, -33.85373, 147.94195, -33.4397, 148.34533, -33.61965, 148.4099

-33.36137, 147.93207, -33.85373, 147.94195
-33.85373, 147.94195, -33.4397, 148.34533
-33.4397, 148.34533, -33.61965, 148.4099

NOTE: Point to point distances using Haversine method.
=> 
54.76
59.28
20.89

54.76 + 59.28 + 20.89
=> 134.93

134.93 - 10 - 0.4
=> 124.53

NOTE: Point to point distances using Vincenty method.
=> 
54.62
59.24
20.84

54.62 + 59.24 + 20.84
=> 134.7

134.7 - 10 - 0.4
=> 124.30
-}
pDay1 :: [Zone]
pDay1 =
    [ Cylinder (Radius $ MkQuantity 100) $ toLL (negate 33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 10000) $ toLL (negate 33.36137, 147.93207)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (negate 33.85373, 147.94195)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (negate 33.4397, 148.34533)
    , Cylinder (Radius $ MkQuantity 400) $ toLL (negate 33.61965, 148.4099)
    ]

dDay1 :: TaskDistance
dDay1 = fromKms [u| 133.357 km |]

{-
NOTE: The task distances show below are taken from the competition *.fsdb file at the
path /Fs/FsCompetition/FsTasks/FsTask/FsTaskScoreParams/FsTaskDistToTp. The first
distance is not 9.9 kms, 10 kms - 100m.

Some flight instruments use WGS84 and others use the FAI spheriod. To accomodate this,
there is a tolerance of either 0.01% or 0.5% used, depending on the competition. For
category 1 events since 2015-01-01 it is 0.01%. Category 2 events can elect to use the
wider margin. This tolerance is used for working out if tracks reach control zones.

The optimised route is worked out in 2D space from a UTM projection. This accounts for
the discrepency with errors coming from choosing wrong waypoints for the optimal route
and from the conversion of these points back to the FAI sphere.

TODO: Find out why the first distance is 9.882 and not 9.9 km.
<FsTaskDistToTp tp_no="1" distance="0.000" />
<FsTaskDistToTp tp_no="2" distance="9.882" />
<FsTaskDistToTp tp_no="3" distance="54.254" />
<FsTaskDistToTp tp_no="4" distance="112.779" />
<FsTaskDistToTp tp_no="5" distance="133.357" />
-}
dsDay1 :: [TaskDistance]
dsDay1 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 9.882
        , 54.254
        , 112.779
        , 133.357
        ]

{-
-33.36137, 147.93207, -32.90223, 147.98492, -32.9536, 147.55457, -33.12592, 147.91043

-33.36137, 147.93207, -32.90223, 147.98492
-32.90223, 147.98492, -32.9536, 147.55457
-32.9536, 147.55457, -33.12592, 147.91043

NOTE: Point to point distances using Haversine method.
=>
51.29
40.57
38.31

51.29 + 40.57 + 38.31
=> 130.17

130.17 - 5 - 0.4
=> 124.77

NOTE: Point to point distances using Vincenty method.
=>
51.16
40.65
38.34

51.16 + 40.65 + 38.34
=> 130.15

130.15 - 5 - 0.4
=> 124.75
-}
pDay2 :: [Zone]
pDay2 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 33.12592, 147.91043))
    ]

dDay2 :: TaskDistance
dDay2 = fromKms [u| 128.284 km |]

dsDay2 :: [TaskDistance]
dsDay2 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 4.891
        , 50.789
        , 90.732
        , 128.284
        ]

{-
-33.36137, 147.93207, -34.02107, 148.2233, -34.11795, 148.5013, -34.82197, 148.66543

-33.36137, 147.93207, -34.02107, 148.2233
-34.02107, 148.2233, -34.11795, 148.5013
-34.11795, 148.5013, -34.82197, 148.66543

NOTE: Point to point distances using Haversine method.
=>
78.15
27.78
79.72

78.15 + 27.78 + 79.72
=> 185.65

185.65 - 25 - 0.4
=> 160.25

NOTE: Point to point distances using Vincenty method.
=>
77.99
27.82
79.54

77.99 + 27.82 + 79.54
=> 185.35

185.35 - 25 - 0.4
=> 159.95
-}
pDay3 :: [Zone]
pDay3 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 34.02107, 148.2233))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 34.11795, 148.5013))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 34.82197, 148.66543))
    ]

dDay3 :: TaskDistance
dDay3 = fromKms [u| 183.856 km |]

dsDay3 :: [TaskDistance]
dsDay3 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 24.854
        , 77.646
        , 105.113
        , 183.856
        ]

{-
-33.36137, 147.93207, -32.90223, 147.98492, -32.46363, 148.989

-33.36137, 147.93207, -32.90223, 147.98492
-32.90223, 147.98492, -32.46363, 148.989

NOTE: Point to point distances using Haversine method.
=>
51.29
105.9

51.29 + 105.9
=> 157.19

157.19 - 15 - 0.4
=> 141.79

NOTE: Point to point distances using Vincenty method.
=>
51.16
106

51.16 + 106
=> 157.16

157.16 - 15 - 0.4
=> 141.76
-}
pDay4 :: [Zone]
pDay4 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 25000) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.46363, 148.989))
    ]

dDay4 :: TaskDistance
dDay4 = fromKms [u| 144.030 km |]

dsDay4 :: [TaskDistance]
dsDay4 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 14.873
        , 26.119
        , 144.030
        ]

{-
-33.36137, 147.93207, -32.56608, 148.22657, -32.0164, 149.43363

-33.36137, 147.93207, -32.56608, 148.22657
-32.56608, 148.22657, -32.0164, 149.43363

NOTE: Point to point distances using Haversine method.
=>
92.6
128.9

92.6 + 128.9
=> 221.5

221.5 - 15 - 0.4
=> 206.1

NOTE: Point to point distances using Vincenty method.
=>
92.4
129

92.4 + 129
=> 221.4

221.4 - 15 - 0.4
=> 206.0
-}
pDay5 :: [Zone]
pDay5 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 32.56608, 148.22657))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.0164, 149.43363))
    ]

dDay5 :: TaskDistance
dDay5 = fromKms [u| 217.389 km |]

dsDay5 :: [TaskDistance]
dsDay5 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 14.873
        , 87.489
        , 217.389
        ]

{-
-33.36137, 147.93207, -32.19498, 147.76218, -31.69323, 148.29623

-33.36137, 147.93207, -32.19498, 147.76218
-32.19498, 147.76218, -31.69323, 148.29623

NOTE: Point to point distances using Haversine method.
=>
130.7
75.18

130.7 + 75.18
=> 205.88 

205.88 - 15 - 0.4
=> 190.48

NOTE: Point to point distances using Vincenty method.
=>
130.3
75.13

130.3 + 75.13
=> 205.43

205.43 - 15 - 0.4
=> 190.03
-}
pDay6 :: [Zone]
pDay6 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 32.19498, 147.76218))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 31.69323, 148.29623))
    ]

dDay6 :: TaskDistance
dDay6 = fromKms [u| 201.822 km |]

dsDay6 :: [TaskDistance]
dsDay6 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 14.873
        , 125.550
        , 201.822
        ]

{-
-33.36137, 147.93207, -32.9536, 147.55457, -32.76052, 148.64958, -32.93585, 148.74947

-33.36137, 147.93207, -32.9536, 147.55457
-32.9536, 147.55457, -32.76052, 148.64958
-32.76052, 148.64958, -32.93585, 148.74947

NOTE: Point to point distances using Haversine method.
=>
57.37
104.5
21.61

57.37 + 104.5 + 21.61
=> 183.48

183.48 - 10 - 0.4
=> 173.08

NOTE: Point to point distances using Vincenty method.
=>
57.32
104.7
21.58

57.32 + 104.7 + 21.58
=> 183.60

183.60 - 10 - 0.4
=> 173.2
-}
pDay7 :: [Zone]
pDay7 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.76052, 148.64958))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 32.93585, 148.74947))
    ]

dDay7 :: TaskDistance
dDay7 = fromKms [u| 174.525 km |]

dsDay7 :: [TaskDistance]
dsDay7 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 9.882
        , 52.259
        , 153.014
        , 174.525
        ]

{-
-33.36137, 147.93207, -33.75343, 147.52865, -33.12908, 147.57323, -33.361, 147.9315

-33.36137, 147.93207, -33.75343, 147.52865
-33.75343, 147.52865, -33.12908, 147.57323
-33.12908, 147.57323, -33.361, 147.9315

NOTE: Point to point distances using Haversine method.
=>
57.43
69.55
42.13

57.43 + 69.55 + 42.13
=> 169.11

169.11 - 10 - 0.4
=> 158.71

NOTE: Point to point distances using Vincenty method.
=>
57.4
69.37
42.15

57.4 + 69.37 + 42.15
=> 168.92

169.92 - 10 - 0.4
=> 159.52
-}
pDay8 :: [Zone]
pDay8 =
    [ Cylinder (Radius $ MkQuantity 100) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius $ MkQuantity 5000) (toLL (negate 33.75343, 147.52865))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 33.12908, 147.57323))
    , Cylinder (Radius $ MkQuantity 400) (toLL (negate 33.361, 147.9315))
    ]

dDay8 :: TaskDistance
dDay8 = fromKms [u| 158.848 km |]

dsDay8 :: [TaskDistance]
dsDay8 =
    fromKms . MkQuantity <$>
        [ 0.000
        , 9.882
        , 52.323
        , 117.028
        , 158.848
        ]
