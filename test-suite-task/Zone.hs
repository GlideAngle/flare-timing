module Zone (zoneUnits, distancePointToPoint, distanceEdgeToEdge) where

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
    , earthRadius
    )

import TestNewtypes

type Pt = (Rational, Rational)

point :: (Rational, Rational) -> Zone
point x = Point (LatLng x)

vector :: (Rational, Rational) -> Zone
vector x = Vector (Bearing 0) (LatLng x)

cylinder :: (Rational, Rational) -> Zone
cylinder x = Cylinder (Radius 1) (LatLng x)

conical :: (Rational, Rational) -> Zone
conical x = Conical (Incline 1) (Radius 1) (LatLng x)

line :: (Rational, Rational) -> Zone
line x = Line (Radius 1) (LatLng x)

semicircle :: (Rational, Rational) -> Zone
semicircle x = SemiCircle (Radius 1) (LatLng x)

zoneUnits :: TestTree
zoneUnits = testGroup "Zone unit tests"
    [ distanceUnits
    , coincidentUnits
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
    [ [ (0, 0), (0, 0) ]
    , [ (1, 0), (1, 0) ]
    , [ (0, 1), (0, 1) ]
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
eps = 1 % 100000000

ptsDisjoint :: [[Pt]]
ptsDisjoint =
    [ [ (0 + eps, 0), (0, 0) ]
    , [ (1 + eps, 0), (1, 0) ]
    , [ (0 + eps, 1), (0, 1) ]
    , [ (1 + eps, 1), (1, 1) ]
    , [ (0, 0 + eps), (0, 0) ]
    , [ (1, 0 + eps), (1, 0) ]
    , [ (0, 1 + eps), (0, 1) ]
    , [ (1, 1 + eps), (1, 1) ]
    , [ (0, 0), (0 + eps, 0) ]
    , [ (1, 0), (1 + eps, 0) ]
    , [ (0, 1), (0 + eps, 1) ]
    , [ (1, 1), (1 + eps, 1) ]
    , [ (0, 0), (0, 0 + eps) ]
    , [ (1, 0), (1, 0 + eps) ]
    , [ (0, 1), (0, 1 + eps) ]
    , [ (1, 1), (1, 1 + eps) ]
    ]

epsR :: Rational
epsR = 1 % 1 + 1 % 100000000

radiiDisjoint :: [[Pt]]
radiiDisjoint =
    [ [ (0 + epsR, 0), (0, 0) ]
    , [ (1 + epsR, 0), (1, 0) ]
    , [ (0 + epsR, 1), (0, 1) ]
    , [ (1 + epsR, 1), (1, 1) ]
    , [ (0, 0 + epsR), (0, 0) ]
    , [ (1, 0 + epsR), (1, 0) ]
    , [ (0, 1 + epsR), (0, 1) ]
    , [ (1, 1 + epsR), (1, 1) ]
    , [ (0, 0), (0 + epsR, 0) ]
    , [ (1, 0), (1 + epsR, 0) ]
    , [ (0, 1), (0 + epsR, 1) ]
    , [ (1, 1), (1 + epsR, 1) ]
    , [ (0, 0), (0, 0 + epsR) ]
    , [ (1, 0), (1, 0 + epsR) ]
    , [ (0, 1), (0, 1 + epsR) ]
    , [ (1, 1), (1, 1 + epsR) ]
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

correct :: [Zone] -> TaskDistance -> Bool
correct _ _ = False

distancePointToPoint :: ZonesTest -> Bool
distancePointToPoint (ZonesTest xs) =
    correct xs $ FS.distancePointToPoint xs

distanceEdgeToEdge :: ZonesTest -> Bool
distanceEdgeToEdge (ZonesTest xs) =
    correct xs $ FS.distancePointToPoint xs
