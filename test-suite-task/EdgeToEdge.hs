module EdgeToEdge (edgeToEdgeUnits) where

import Data.Ratio((%))
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
    , Epsilon(..)
    , degToRadLL
    )

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

mkDayUnits :: TestName -> [Zone] -> Double -> TestTree
mkDayUnits title pDay dDay = testGroup title
    [ HU.testCase "zones are separated" $ FS.separatedZones pDay @?= True
    , HU.testCase
            ( "point-to-point distance >= edge-to-edge distance, "
            ++ show ppDay ++ " m"
            ++ " >= "
            ++ show eeDay ++ " m"
            ) $
        (ppDay >= eeDay) @? ppDay .>=. eeDay
    , HU.testCase ("point-to-point distance >= " ++ show dDay ++ " m") $
        (ppDay >= distDay) @? ppDay .>=. distDay
    , HU.testCase ("edge-to-edge distance <= " ++ show dDay ++ " m") $
        (eeDay <= distDay) @? eeDay .<=. distDay
    ]
    where
        distDay = toDist dDay
        ppDay = FS.distancePointToPoint pDay
        eeDay = fst $ FS.distanceEdgeToEdge samples mm30 pDay

day1Units :: TestTree
day1Units = mkDayUnits "Task 1" pDay1 dDay1

day2Units :: TestTree
day2Units = mkDayUnits "Task 2" pDay2 dDay2

day3Units :: TestTree
day3Units = mkDayUnits "Task 3" pDay3 dDay3

day4Units :: TestTree
day4Units = mkDayUnits "Task 4" pDay4 dDay4

day5Units :: TestTree
day5Units = mkDayUnits "Task 5" pDay5 dDay5

day6Units :: TestTree
day6Units = mkDayUnits "Task 6" pDay6 dDay6

day7Units :: TestTree
day7Units = mkDayUnits "Task 7" pDay7 dDay7

day8Units :: TestTree
day8Units = mkDayUnits "Task 8" pDay8 dDay8

pDay1 :: [Zone]
pDay1 =
    --[ Cylinder (Radius 100) $ toLL (negate 33.36137, 147.93207)
    [ Cylinder (Radius 10000) $ toLL (negate 33.36137, 147.93207)
    , Cylinder (Radius 400) $ toLL (negate 33.85373, 147.94195)
    , Cylinder (Radius 400) $ toLL (negate 33.4397, 148.34533)
    , Cylinder (Radius 400) $ toLL (negate 33.61965, 148.4099)
    ]

dDay1 :: Double
dDay1 = 1000 * 133.357 + 10000 + 400

dsDay1 :: [Double]
dsDay1 = (* 1000) <$> [ 0.000, 9.882, 54.254, 112.779, 133.357 ]

pDay2 :: [Zone]
pDay2 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 5000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 400) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius 400) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius 400) (toLL (negate 33.12592, 147.91043))
    ]

dDay2 :: Double
dDay2 = 1000 * 128.284 + 5000 + 400

dsDay2 :: [Double]
dsDay2 = (* 1000) <$> [ 0.000, 4.891, 50.789, 90.732, 128.284 ]

pDay3 :: [Zone]
pDay3 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 25000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 400) (toLL (negate 34.02107, 148.2233))
    , Cylinder (Radius 400) (toLL (negate 34.11795, 148.5013))
    , Cylinder (Radius 400) (toLL (negate 34.82197, 148.66543))
    ]

dDay3 :: Double
dDay3 = 1000 * 183.856 + 25000 + 400

dsDay3 :: [Double]
dsDay3 = (* 1000) <$> [ 0.000, 24.854, 77.646, 105.113, 183.856 ]

pDay4 :: [Zone]
pDay4 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 25000) (toLL (negate 32.90223, 147.98492))
    , Cylinder (Radius 400) (toLL (negate 32.46363, 148.989))
    ]

dDay4 :: Double
dDay4 = 1000 * 144.030 + 15000 + 400

dsDay4 :: [Double]
dsDay4 = (* 1000) <$> [ 0.000, 14.873, 26.119, 144.030 ]

pDay5 :: [Zone]
pDay5 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 32.56608, 148.22657))
    , Cylinder (Radius 400) (toLL (negate 32.0164, 149.43363))
    ]

dDay5 :: Double
dDay5 = 1000 * 217.389 + 15000 + 400

dsDay5 :: [Double]
dsDay5 = (* 1000) <$> [ 0.000, 14.873, 87.489, 217.389 ]

pDay6 :: [Zone]
pDay6 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 15000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 32.19498, 147.76218))
    , Cylinder (Radius 400) (toLL (negate 31.69323, 148.29623))
    ]

dDay6 :: Double
dDay6 = 1000 * 201.822 + 15000 + 400

dsDay6 :: [Double]
dsDay6 = (* 1000) <$> [ 0.000, 14.873, 125.550, 201.822 ]

pDay7 :: [Zone]
pDay7 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 32.9536, 147.55457))
    , Cylinder (Radius 400) (toLL (negate 32.76052, 148.64958))
    , Cylinder (Radius 400) (toLL (negate 32.93585, 148.74947))
    ]

dDay7 :: Double
dDay7 = 1000 * 174.525 + 10000 + 400

dsDay7 :: [Double]
dsDay7 = (* 1000) <$> [ 0.000,  9.882, 52.259, 153.014, 174.525 ]

pDay8 :: [Zone]
pDay8 =
    --[ Cylinder (Radius 100) (toLL (negate 33.36137, 147.93207))
    [ Cylinder (Radius 10000) (toLL (negate 33.36137, 147.93207))
    , Cylinder (Radius 5000) (toLL (negate 33.75343, 147.52865))
    , Cylinder (Radius 400) (toLL (negate 33.12908, 147.57323))
    , Cylinder (Radius 400) (toLL (negate 33.361, 147.9315))
    ]

dDay8 :: Double
dDay8 = 1000 * 158.848 + 10000 + 400

dsDay8 :: [Double]
dsDay8 = (* 1000) <$> [ 0.000, 9.882, 52.323, 117.028, 158.848 ]

{--
    Zone edge shortest path unit tests
      Forbes 2011/2012 distances
        Task 1
          zones are separated:                   OK
          point-to-point distance >= 143757.0 m: FAIL
            expected: d = 143757.0
             but got: d = 141189.87681083506
          edge-to-edge distance <= 143757.0 m:   FAIL (32.32s)
            expected: d = 143757.0
             but got: d = 141189.87681083506
        Task 2
          zones are separated:                   OK
          point-to-point distance >= 133684.0 m: FAIL
            expected: d = 133684.0
             but got: d = 135338.5618397564
          edge-to-edge distance <= 133684.0 m:   FAIL (32.17s)
            expected: d = 133684.0
             but got: d = 128775.82172728535
        Task 3
          zones are separated:                   OK
          point-to-point distance >= 209256.0 m: FAIL
            expected: d = 209256.0
             but got: d = 194232.84019652175
          edge-to-edge distance <= 209256.0 m:   FAIL (33.12s)
            expected: d = 209256.0
             but got: d = 194232.84019652175
        Task 4
          zones are separated:                   OK
          point-to-point distance >= 159430.0 m: FAIL
            expected: d = 159430.0
             but got: d = 163428.7020747752
          edge-to-edge distance <= 159430.0 m:   FAIL (22.33s)
            expected: d = 159430.0
             but got: d = 136010.66433077582
        Task 5
          zones are separated:                   OK
          point-to-point distance >= 232789.0 m: FAIL
            expected: d = 232789.0
             but got: d = 230559.15743766545
          edge-to-edge distance <= 232789.0 m:   FAIL (23.62s)
            expected: d = 232789.0
             but got: d = 213200.4644610034
        Task 6
          zones are separated:                   OK
          point-to-point distance >= 217222.0 m: FAIL
            expected: d = 217222.0
             but got: d = 215553.37688114512
          edge-to-edge distance <= 217222.0 m:   FAIL (23.94s)
            expected: d = 217222.0
             but got: d = 197036.60024378996
        Task 7
          zones are separated:                   OK
          point-to-point distance >= 184925.0 m: FAIL
            expected: d = 184925.0
             but got: d = 190216.1771138789
          edge-to-edge distance <= 184925.0 m:   FAIL (32.98s)
            expected: d = 184925.0
             but got: d = 173097.7329550498
        Task 8
          zones are separated:                   OK
          point-to-point distance >= 169248.0 m: FAIL
            expected: d = 169248.0
             but got: d = 176579.53428195586
          edge-to-edge distance <= 169248.0 m:   FAIL (33.28s)
            expected: d = 169248.0
             but got: d = 173998.44651692512
             --}
