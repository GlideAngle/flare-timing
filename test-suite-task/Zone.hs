module Zone
    ( zoneUnits
    , distancePoint
    , distanceEdge
    , distanceLess
    ) where

import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Task as FS
import Flight.Task
    ( ShowAngle(..)
    , Zone(..)
    , LatLng(..)
    , Radius(..)
    , Incline (..)
    , Bearing(..)
    , TaskDistance(..)
    , Samples(..)
    , Tolerance(..)
    , earthRadius
    , center
    , separatedZones
    )

import TestNewtypes

type Pt = (Rational, Rational)

point :: (Rational, Rational) -> Zone
point x = Point (LatLng x)

vector :: (Rational, Rational) -> Zone
vector x = Vector (Bearing 0) (LatLng x)

cylinder :: (Rational, Rational) -> Zone
cylinder x = Cylinder (Radius earthRadius) (LatLng x)

conical :: (Rational, Rational) -> Zone
conical x = Conical (Incline 1) (Radius earthRadius) (LatLng x)

line :: (Rational, Rational) -> Zone
line x = Line (Radius earthRadius) (LatLng x)

semicircle :: (Rational, Rational) -> Zone
semicircle x = SemiCircle (Radius earthRadius) (LatLng x)

zoneUnits :: TestTree
zoneUnits = testGroup "Zone unit tests"
    [ distanceUnits
    , coincidentUnits
    , touchingUnits
    , disjointUnits
    ]

distanceUnits :: TestTree
distanceUnits = testGroup "Point-to-point distance"
    [ emptyDistance
    , pointDistance
    , vectorDistance
    , cylinderDistance
    , conicalDistance
    , lineDistance
    , semicircleDistance
    ]

coincidentUnits :: TestTree
coincidentUnits = testGroup "Coincident zone separation"
    [ pointCoincident
    , vectorCoincident
    , cylinderCoincident
    , conicalCoincident
    , lineCoincident
    , semicircleCoincident
    ]

touchingUnits :: TestTree
touchingUnits = testGroup "Touching zone separation"
    [ cylinderTouching
    , conicalTouching
    , lineTouching
    , semicircleTouching
    ]

disjointUnits :: TestTree
disjointUnits = testGroup "Disjoint zone separation"
    [ pointDisjoint
    , vectorDisjoint
    , cylinderDisjoint
    , conicalDisjoint
    , lineDisjoint
    , semicircleDisjoint
    ]

emptyDistance :: TestTree
emptyDistance = testGroup "Point-to-point distance"
    [ HU.testCase "No zones = zero point-to-point distance" $
        FS.distancePointToPoint [] @?= TaskDistance 0
    ]

toDistance :: String -> [[Zone]] -> TestTree
toDistance title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "distance ", showRadian x, " = earth radius" ]) $
                FS.distancePointToPoint x
                    @?= TaskDistance earthRadius

ptsDistance :: [[Pt]]
ptsDistance =
    [ [ (1, 0), (0, 0) ]
    , [ (0, 1), (0, 0) ]
    , [ (0, 0), (0, 1) ]
    , [ (0, 0), (1, 0) ]
    ]

pointDistance :: TestTree
pointDistance = toDistance "Distance over point zones" ((fmap . fmap) point ptsDistance)

vectorDistance :: TestTree
vectorDistance = toDistance "Distance over vector zones" ((fmap . fmap) vector ptsDistance)

cylinderDistance :: TestTree
cylinderDistance = toDistance "Distance over cylinder zones" ((fmap . fmap) cylinder ptsDistance)

conicalDistance :: TestTree
conicalDistance = toDistance "Distance over conical zones" ((fmap . fmap) conical ptsDistance)

lineDistance :: TestTree
lineDistance = toDistance "Distance over line zones" ((fmap . fmap) line ptsDistance)

semicircleDistance :: TestTree
semicircleDistance = toDistance "Distance over semicircle zones" ((fmap . fmap) semicircle ptsDistance)

coincident :: String -> [[Zone]] -> TestTree
coincident title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "concident pair of "
                                 , showRadian $ head x
                                 , " = not separate"
                                 ]) $
                FS.separatedZones x
                    @?= False

ptsCoincident :: [[Pt]]
ptsCoincident =
    [ [ (1, 0), (1, 0) ]
    , [ (0, 1), (0, 1) ]
    , [ (1, 0), (1, 0) ]
    , [ (1, 1), (1, 1) ]
    ]

pointCoincident :: TestTree
pointCoincident = coincident "Point zones" ((fmap . fmap) point ptsCoincident)

vectorCoincident :: TestTree
vectorCoincident = coincident "Vector zones" ((fmap . fmap) vector ptsCoincident)

cylinderCoincident :: TestTree
cylinderCoincident = coincident "Cylinder zones" ((fmap . fmap) cylinder ptsCoincident)

conicalCoincident :: TestTree
conicalCoincident = coincident "Conical zones" ((fmap . fmap) conical ptsCoincident)

lineCoincident :: TestTree
lineCoincident = coincident "Line zones" ((fmap . fmap) line ptsCoincident)

semicircleCoincident :: TestTree
semicircleCoincident = coincident "Semicircle zones" ((fmap . fmap) semicircle ptsCoincident)

touching :: String -> [[Zone]] -> TestTree
touching title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "touching pair of "
                                 , showRadian x
                                 , " = not separate"
                                 ]) $
                FS.separatedZones x
                    @?= False

epsM :: Rational
epsM = 2 % 1 - 1 % 100000000

radiiTouching :: [[Pt]]
radiiTouching =
    [ [ (0, epsM), (0, 0) ]
    , [ (0, negate epsM), (0, 0) ]
    ]

cylinderTouching :: TestTree
cylinderTouching = touching "Cylinder zones" ((fmap . fmap) cylinder radiiTouching)

conicalTouching :: TestTree
conicalTouching = touching "Conical zones" ((fmap . fmap) conical radiiTouching)

lineTouching :: TestTree
lineTouching = touching "Line zones" ((fmap . fmap) line radiiTouching)

semicircleTouching :: TestTree
semicircleTouching = touching "Semicircle zones" ((fmap . fmap) semicircle radiiTouching)

disjoint :: String -> [[Zone]] -> TestTree
disjoint title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "disjoint pair of "
                                 , showRadian x 
                                 , " = separate"
                                 ]) $
                FS.separatedZones x
                    @?= True

eps :: Rational
eps = 2 % 1 + 1 % 100000000

ptsDisjoint :: [[Pt]]
ptsDisjoint =
    [ [ (0, eps), (0, 0) ]
    , [ (0, negate eps), (1, 0) ]
    ]

epsR :: Rational
epsR = 2 % 1 + 1 % 100000000

radiiDisjoint :: [[Pt]]
radiiDisjoint =
    [ [ (0, epsR), (0, 0) ]
    , [ (0, negate epsR), (0, 0) ]
    ]

pointDisjoint :: TestTree
pointDisjoint = disjoint "Point zones" ((fmap . fmap) point ptsDisjoint)

vectorDisjoint :: TestTree
vectorDisjoint = disjoint "Vector zones" ((fmap . fmap) vector ptsDisjoint)

cylinderDisjoint :: TestTree
cylinderDisjoint = disjoint "Cylinder zones" ((fmap . fmap) cylinder radiiDisjoint)

conicalDisjoint :: TestTree
conicalDisjoint = disjoint "Conical zones" ((fmap . fmap) conical radiiDisjoint)

lineDisjoint :: TestTree
lineDisjoint = disjoint "Line zones" ((fmap . fmap) line radiiDisjoint)

semicircleDisjoint :: TestTree
semicircleDisjoint = disjoint "Semicircle zones" ((fmap . fmap) semicircle radiiDisjoint)

correctPoint :: [Zone] -> TaskDistance -> Bool
correctPoint [] (TaskDistance d) = d == 0
correctPoint [_] (TaskDistance d) = d == 0
correctPoint xs (TaskDistance d)
    | and $ map (== head ys) (tail ys) = d == 0
    | otherwise = d > 0
    where
        ys = center <$> xs

correctEdge :: [Zone] -> TaskDistance -> Bool
correctEdge [] (TaskDistance d) = d == 0
correctEdge [_] (TaskDistance d) = d == 0
correctEdge xs (TaskDistance d)
    | and $ map (== head ys) (tail ys) = d == 0
    | not $ separatedZones xs = d == 0
    | otherwise = d > 0
    where
        ys = center <$> xs

distancePoint :: ZonesTest -> Bool
distancePoint (ZonesTest xs) =
    correctPoint xs $ FS.distancePointToPoint xs

samples :: Samples
samples = Samples 10

mm10 :: Tolerance
mm10 = Tolerance $ 10 % 1000

distanceEdge :: ZonesTest -> Bool
distanceEdge (ZonesTest xs) =
    (\(d, _) -> correctEdge xs d) $ FS.distanceEdgeToEdge samples mm10 xs

distanceLess :: ZonesTest -> Bool
distanceLess (ZonesTest xs)
    | length xs < 3 = True
    | otherwise =
        dEdge <= dPoint
        where
            (TaskDistance dEdge, _) = FS.distanceEdgeToEdge samples mm10 xs
            TaskDistance dPoint = FS.distancePointToPoint xs
