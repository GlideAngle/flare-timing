module Zone (zoneUnits, distancePointToPoint, distanceEdgeToEdge) where

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
vector x = Vector (LatLng x) (Bearing 0)

cylinder :: (Rational, Rational) -> Zone
cylinder x = Cylinder (LatLng x) (Radius 1)

conical :: (Rational, Rational) -> Zone
conical x = Conical (LatLng x) (Radius 1) (Incline 1)

line :: (Rational, Rational) -> Zone
line x = Line (LatLng x) (Radius 1)

semicircle :: (Rational, Rational) -> Zone
semicircle x = SemiCircle (LatLng x) (Radius 1)

pts :: [[Pt]]
pts =
    [ [ (1, 0), (0, 0) ]
    , [ (0, 1), (0, 0) ]
    , [ (0, 0), (0, 1) ]
    , [ (0, 0), (1, 0) ]
    ]

zoneUnits :: TestTree
zoneUnits = testGroup "Point-to-point distance"
    [ emptyUnits
    , pointUnits
    , vectorUnits
    , cylinderUnits
    , conicalUnits
    , lineUnits
    , semicircleUnits
    ]

emptyUnits :: TestTree
emptyUnits = testGroup "Point-to-point distance"
    [ HU.testCase "No zones = zero point-to-point distance" $
        FS.distancePointToPoint [] @?= TaskDistance 0
    ]

toPointUnits :: String -> [[Zone]] -> TestTree
toPointUnits title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "distance ", showDegree x, " = earth radius" ]) $
                FS.distancePointToPoint x
                    @?= TaskDistance earthRadius

pointUnits :: TestTree
pointUnits = toPointUnits "Point zones" ((fmap . fmap) point pts)

vectorUnits :: TestTree
vectorUnits = toPointUnits "Vector zones" ((fmap . fmap) vector pts)

cylinderUnits :: TestTree
cylinderUnits = toPointUnits "Cylinder zones" ((fmap . fmap) cylinder pts)

conicalUnits :: TestTree
conicalUnits = toPointUnits "Conical zones" ((fmap . fmap) conical pts)

lineUnits :: TestTree
lineUnits = toPointUnits "Line zones" ((fmap . fmap) line pts)

semicircleUnits :: TestTree
semicircleUnits = toPointUnits "Semicircle zones" ((fmap . fmap) semicircle pts)

correct :: [Zone] -> TaskDistance -> Bool
correct _ _ = False

distancePointToPoint :: ZonesTest -> Bool
distancePointToPoint (ZonesTest xs) =
    correct xs $ FS.distancePointToPoint xs

distanceEdgeToEdge :: ZonesTest -> Bool
distanceEdgeToEdge (ZonesTest xs) =
    correct xs $ FS.distancePointToPoint xs
