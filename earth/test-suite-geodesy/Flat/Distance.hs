module Flat.Distance
    ( units
    , distancePoint
    , distanceEuclidean
    , distanceEuclideanF
    ) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (QTaskDistance, TaskDistance(..), PathDistance(..))
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , Incline (..)
    , Bearing(..)
    , center
    )
import Flight.Zone.Path (distancePointToPoint)
import Flight.Earth.Sphere (earthRadius)

import Props.Zone (ZonesTest(..))
import Props.Euclidean (EuclideanTest(..))
import Flat.Span (spanD, spanR)

units :: TestTree
units =
    testGroup "Point-to-point distance"
    [ emptyDistance
    , pointDistance
    , vectorDistance
    , cylinderDistance
    , conicalDistance
    , lineDistance
    , semicircleDistance
    ]

type Pt = (Rational, Rational)

toLL :: (Rational, Rational) -> LatLng Rational [u| rad |]
toLL (lat, lng) =
    LatLng (Lat lat', Lng lng')
    where
        lat' = MkQuantity lat
        lng' = MkQuantity lng

point :: (Rational, Rational) -> Zone Rational
point x =
    Point $ toLL x

vector :: (Rational, Rational) -> Zone Rational
vector x =
    Vector (Bearing zero) (toLL x) 

cylinder :: (Rational, Rational) -> Zone Rational
cylinder x =
    Cylinder earthRadius (toLL x)

conical :: (Rational, Rational) -> Zone Rational
conical x =
    Conical (Incline $ MkQuantity 1) earthRadius (toLL x)

line :: (Rational, Rational) -> Zone Rational
line x =
    Line Nothing earthRadius (toLL x)

semicircle :: (Rational, Rational) -> Zone Rational
semicircle x =
    SemiCircle Nothing earthRadius (toLL x)

emptyDistance :: TestTree
emptyDistance = testGroup "Point-to-point distance"
    [ HU.testCase "No zones = zero point-to-point distance" $
        edgesSum (distancePointToPoint spanR []) @?= (TaskDistance $ MkQuantity 0)
    ]

toDistance :: String -> [[Zone Rational]] -> TestTree
toDistance title xs =
    testGroup title (f <$> xs)
    where
        Radius eR = earthRadius
        f x =
            HU.testCase (mconcat [ "distance ", show x, " = earth radius" ]) $
                edgesSum (distancePointToPoint spanR x)
                    @?= TaskDistance eR

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

correctPoint :: [Zone Rational] -> QTaskDistance Rational [u| m |] -> Bool
correctPoint [] (TaskDistance (MkQuantity d)) = d == 0
correctPoint [_] (TaskDistance (MkQuantity d)) = d == 0
correctPoint [Cylinder xR x, Cylinder yR y] (TaskDistance (MkQuantity d))
    | x == y = (xR == yR && d == 0) || d > 0
    | otherwise = d > 0
correctPoint xs (TaskDistance (MkQuantity d))
    | all (== head ys) (tail ys) = d == 0
    | otherwise = d > 0
    where
        ys = center <$> xs

distanceEuclideanF :: EuclideanTest Double -> Bool
distanceEuclideanF (EuclideanTest (x, y)) =
    [u| 0 m |] <= d
    where
        TaskDistance d = spanD x y

distanceEuclidean :: EuclideanTest Rational -> Bool
distanceEuclidean (EuclideanTest (x, y)) =
    [u| 0 m |] <= d
    where
        TaskDistance d = spanR x y

distancePoint :: ZonesTest Rational -> Bool
distancePoint (ZonesTest xs) =
    (\(PathDistance d _) -> correctPoint xs d)
    $ distancePointToPoint spanR xs
