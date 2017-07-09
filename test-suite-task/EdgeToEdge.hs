module EdgeToEdge (edgeToEdgeUnits) where

import Data.Ratio((%))
import Numeric (showFFloat)
import Data.List (inits)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit as HU ((@?=), (@?), testCase)

import qualified Flight.Task as FS
import Flight.Task
    ( LatLng(..)
    , Radius(..)
    , Samples(..)
    , Tolerance(..)
    , Zone(..)
    , TaskDistance(..)
    , EdgeDistance(..)
    , Epsilon(..)
    , degToRadLL
    )

showKm :: Double -> String
showKm x =
    showFFloat (Just 3) (x / 1000) ""

showKms :: [Double] -> String
showKms xs =
    show $ showKm <$> xs

(.>=.) :: (Show a, Show b) => a -> b -> String
(.>=.) x y = show x ++ " >= " ++ show y

(.<=.) :: (Show a, Show b) => a -> b -> String
(.<=.) x y = show x ++ " <= " ++ show y

edgeToEdgeUnits :: TestTree
edgeToEdgeUnits = testGroup "Zone edge shortest path unit tests"
    [ circumSampleUnits
    , forbesUnits
    ]

mm100 :: Tolerance
mm100 = Tolerance $ 100 % 1000

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

mm1 :: Tolerance
mm1 = Tolerance $ 1 % 1000

samples :: Samples
samples = Samples 100

circumSampleUnits :: TestTree
circumSampleUnits = testGroup "Points just within the zone"
    [ HU.testCase "No points generated outside a 40m cylinder when within 1mm" $
        filter (> 40)
        (snd $ FS.circumSample samples mm1 (Radius 40) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 400m cylinder when within 1mm" $
        filter (> 400)
        (snd $ FS.circumSample samples mm1 (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 1km cylinder when within 30mm" $
        filter (> 1000) 
        (snd $ FS.circumSample samples mm30 (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 10km cylinder when within 100mm" $
        filter (> 10000)
        (snd $ FS.circumSample samples mm100 (Radius 10000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 100km cylinde when within 100mm" $
        filter (> 100000)
        (snd $ FS.circumSample samples mm100 (Radius 100000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points generated outside a 1000km cylinder when within 100mm" $
        filter (> 1000000)
        (snd $ FS.circumSample samples mm100 (Radius 1000000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1mm from a 40m cylinder" $
        filter (< 39)
        (snd $ FS.circumSample samples mm1 (Radius 40) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 1mm from a 400m cylinder" $
        filter (< 399)
        (snd $ FS.circumSample samples mm1 (Radius 400) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 30mm from a 1km cylinder" $
        filter (< 999)
        (snd $ FS.circumSample samples mm30 (Radius 1000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 100m from a 10km cylinder" $
        filter (< 9999)
        (snd $ FS.circumSample samples mm100 (Radius 10000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 100m from a 100km cylinder" $
        filter (< 99999)
        (snd $ FS.circumSample samples mm100 (Radius 100000) (LatLng (1, 1)))
        @?= [] 

    , HU.testCase "No points further than 100m from a 1000km cylinder" $
        filter (< 999999)
        (snd $ FS.circumSample samples mm100 (Radius 1000000) (LatLng (1, 1)))
        @?= [] 
    ]

eps :: Epsilon
eps = Epsilon $ 2 % 1 + 1 % 100000000

toLL :: (Double, Double) -> LatLng
toLL (lat, lng) = degToRadLL eps $ LatLng (toRational lat, toRational lng)

toDist :: Double -> TaskDistance
toDist x = TaskDistance $ toRational x

forbesUnits :: TestTree
forbesUnits = testGroup "Forbes 2011/2012 distances"
    [ day1Units
    , day2Units
    , day3Units
    , day4Units
    , day5Units
    , day6Units
    , day7Units
    , day8Units
    ]

mkDayUnits :: TestName -> [Zone] -> Double -> [Double] -> TestTree
mkDayUnits title pDay dDay dsDay = testGroup title
    [ HU.testCase "zones are separated" $ FS.separatedZones pDay @?= True

    , HU.testCase
        ( "point-to-point distance >= edge-to-edge distance, "
        ++ (showKm $ fromRational ppDay')
        ++ " >= "
        ++ (showKm $ fromRational eeDay')
        )
        $ (ppDay >= eeDay) @? ppDay .>=. eeDay

    , HU.testCase
        ("point-to-point distance >= " ++ showKm dDay)
        $ (ppDay >= distDay) @? ppDay .>=. distDay

    , HU.testCase
        ("edge-to-edge distance <= " ++ showKm dDay)
        $ (eeDay <= distDay) @? eeDay .<=. distDay

    , HU.testCase
        ("point-to-point distances "
        ++ showKms ppDayInits
        ++ " >= "
        ++ showKms dsDay
        ) $
        (ppDayInits >= dsDay) @? ppDayInits .>=. dsDay

    , HU.testCase
        ("edge-to-edge distances "
        ++ showKms eeDayInits
        ++ " <= "
        ++ showKms dsDay
        ) $
        (eeDayInits <= dsDay) @? eeDayInits .<=. dsDay
    ]
    where
        distDay = toDist dDay
        pp = FS.distancePointToPoint
        ee = FS.distanceEdgeToEdge samples mm30
        ppDay@(TaskDistance ppDay') = pp pDay
        eeDay@(TaskDistance eeDay') = edges $ ee pDay
        pDayInits = drop 1 $ inits pDay
        unDist (TaskDistance x) = fromRational x :: Double
        ppDayInits = unDist . pp <$> pDayInits
        eeDayInits = unDist . edges . ee <$> pDayInits

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

-33.36137, 147.93207, -33.85373, 147.94195, -33.4397, 148.34533, -33.61965, 148.4099

-33.36137, 147.93207, -33.85373, 147.94195
-33.85373, 147.94195, -33.4397, 148.34533
-33.4397, 148.34533, -33.61965, 148.4099

NOTE: Point to point distances using Vincenty method.
=> 
54.76
59.28
20.89

54.76 + 59.28 + 20.89
=> 134.93

134.93 - 10 - 0.4
=> 124.53
-}
pDay1 :: [Zone]
pDay1 =
    --[ Cylinder (Radius 100) $ toLL (negate 33.36137, 147.93207)
    [ Cylinder (Radius 10000) $ toLL (negate 33.36137, 147.93207)
    , Cylinder (Radius 400) $ toLL (negate 33.85373, 147.94195)
    , Cylinder (Radius 400) $ toLL (negate 33.4397, 148.34533)
    , Cylinder (Radius 400) $ toLL (negate 33.61965, 148.4099)
    ]

dDay1 :: Double
dDay1 = 1000 * 133.357

dsDay1 :: [Double]
dsDay1 = (* 1000) <$> [ 0.000, 9.882, 54.254, 112.779, 133.357 ]

{-
-33.36137, 147.93207, -32.90223, 147.98492, -32.9536, 147.55457, -33.12592, 147.91043

-33.36137, 147.93207, -32.90223, 147.98492
-32.90223, 147.98492, -32.9536, 147.55457
-32.9536, 147.55457, -33.12592, 147.91043

NOTE: Point to point distances using Vincenty method.
=>
51.29
40.57
38.31

51.29 + 40.57 + 38.31
=> 130.17

130.17 - 5 - 0.4
=> 124.77
-}
pDay2 :: [Zone]
pDay2 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 5000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 400) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius 400) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius 400) (toLL (negate 33.12592, 147.91043))
    ]

dDay2 :: Double
dDay2 = 1000 * 128.284

dsDay2 :: [Double]
dsDay2 = (* 1000) <$> [ 0.000, 4.891, 50.789, 90.732, 128.284 ]

{-
-33.36137, 147.93207, -34.02107, 148.2233, -34.11795, 148.5013, -34.82197, 148.66543

-33.36137, 147.93207, -34.02107, 148.2233
-34.02107, 148.2233, -34.11795, 148.5013
-34.11795, 148.5013, -34.82197, 148.66543

NOTE: Point to point distances using Vincenty method.
=>
78.15
27.78
79.72

78.15 + 27.78 + 79.72
=> 185.65

185.65 - 25 - 0.4
=> 160.25
-}
pDay3 :: [Zone]
pDay3 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 25000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 400) (toLL (negate 34.02107, 148.2233))
    , Cylinder (Radius 400) (toLL (negate 34.11795, 148.5013))
    , Cylinder (Radius 400) (toLL (negate 34.82197, 148.66543))
    ]

dDay3 :: Double
dDay3 = 1000 * 183.856

dsDay3 :: [Double]
dsDay3 = (* 1000) <$> [ 0.000, 24.854, 77.646, 105.113, 183.856 ]

{-
-33.36137, 147.93207, -32.90223, 147.98492, -32.46363, 148.989

-33.36137, 147.93207, -32.90223, 147.98492
-32.90223, 147.98492, -32.46363, 148.989

NOTE: Point to point distances using Vincenty method.
=>
51.29
105.9

51.29 + 105.9
=> 157.19

157.19 - 15 - 0.4
=> 141.79
-}
pDay4 :: [Zone]
pDay4 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 25000) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius 400) (toLL (negate 32.46363, 148.989))
    ]

dDay4 :: Double
dDay4 = 1000 * 144.030

dsDay4 :: [Double]
dsDay4 = (* 1000) <$> [ 0.000, 14.873, 26.119, 144.030 ]

{-
-33.36137, 147.93207, -32.56608, 148.22657, -32.0164, 149.43363

-33.36137, 147.93207, -32.56608, 148.22657
-32.56608, 148.22657, -32.0164, 149.43363

NOTE: Point to point distances using Vincenty method.
=>
92.6
128.9

92.6 + 128.9
=> 221.5

221.5 - 15 - 0.4
=> 206.1
-}
pDay5 :: [Zone]
pDay5 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 32.56608, 148.22657))
    , Cylinder (Radius 400) (toLL (negate 32.0164, 149.43363))
    ]

dDay5 :: Double
dDay5 = 1000 * 217.389

dsDay5 :: [Double]
dsDay5 = (* 1000) <$> [ 0.000, 14.873, 87.489, 217.389 ]

{-
-33.36137, 147.93207, -32.19498, 147.76218, -31.69323, 148.29623

-33.36137, 147.93207, -32.19498, 147.76218
-32.19498, 147.76218, -31.69323, 148.29623

NOTE: Point to point distances using Vincenty method.
=>
130.7
75.18

130.7 + 75.18
=> 205.88 

205.88 - 15 - 0.4
=> 190.48
-}
pDay6 :: [Zone]
pDay6 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 32.19498, 147.76218))
    , Cylinder (Radius 400) (toLL (negate 31.69323, 148.29623))
    ]

dDay6 :: Double
dDay6 = 1000 * 201.822

dsDay6 :: [Double]
dsDay6 = (* 1000) <$> [ 0.000, 14.873, 125.550, 201.822 ]

{-
-33.36137, 147.93207, -32.9536, 147.55457, -32.76052, 148.64958, -32.93585, 148.74947

-33.36137, 147.93207, -32.9536, 147.55457
-32.9536, 147.55457, -32.76052, 148.64958
-32.76052, 148.64958, -32.93585, 148.74947

NOTE: Point to point distances using Vincenty method.
=>
57.37
104.5
21.61

57.37 + 104.5 + 21.61
=> 183.48

183.48 - 10 - 0.4
=> 173.08

-}
pDay7 :: [Zone]
pDay7 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius 400) (toLL (negate 32.76052, 148.64958))
    , Cylinder (Radius 400) (toLL (negate 32.93585, 148.74947))
    ]

dDay7 :: Double
dDay7 = 1000 * 174.525

dsDay7 :: [Double]
dsDay7 = (* 1000) <$> [ 0.000,  9.882, 52.259, 153.014, 174.525 ]

{-
-33.36137, 147.93207, -33.75343, 147.52865, -33.12908, 147.57323, -33.361, 147.9315

-33.36137, 147.93207, -33.75343, 147.52865
-33.75343, 147.52865, -33.12908, 147.57323
-33.12908, 147.57323, -33.361, 147.9315

NOTE: Point to point distances using Vincenty method.
=>
57.43
69.55
42.13

57.43 + 69.55 + 42.13
=> 169.11

169.11 - 10 - 0.4
=> 158.71
-}
pDay8 :: [Zone]
pDay8 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 33.75343, 147.52865))
    , Cylinder (Radius 400) (toLL (negate 33.12908, 147.57323))
    , Cylinder (Radius 400) (toLL (negate 33.361, 147.9315))
    ]

dDay8 :: Double
dDay8 = 1000 * 158.848

dsDay8 :: [Double]
dsDay8 = (* 1000) <$> [ 0.000, 9.882, 52.323, 117.028, 158.848 ]
