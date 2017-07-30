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

module Zone
    ( zoneUnits
    , distancePoint
    , distanceEdge
    , distanceLess
    ) where

import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Flight.Task as FS
import Flight.Task
    ( Zone(..)
    , Lat(..)
    , Lng(..)
    , LatLng(..)
    , Radius(..)
    , Incline (..)
    , Bearing(..)
    , TaskDistance(..)
    , EdgeDistance(..)
    , Tolerance(..)
    , DistancePath(..)
    , earthRadius
    , center
    , separatedZones
    , radToDegLL
    , defEps
    )

import TestNewtypes

type Pt = (Rational, Rational)

toLL :: (Rational, Rational) -> LatLng [u| deg |]
toLL (lat, lng) =
    radToDegLL defEps (LatLng (Lat lat', Lng lng'))
    where
        lat' = MkQuantity lat
        lng' = MkQuantity lng

point :: (Rational, Rational) -> Zone [u| deg |]
point x =
    Point $ toLL x

vector :: (Rational, Rational) -> Zone [u| deg |]
vector x =
    Vector (Bearing zero) (toLL x) 

cylinder :: (Rational, Rational) -> Zone [u| deg |]
cylinder x =
    Cylinder (Radius earthRadius) (toLL x)

conical :: (Rational, Rational) -> Zone [u| deg |]
conical x =
    Conical (Incline $ MkQuantity 1) (Radius earthRadius) (toLL x)

line :: (Rational, Rational) -> Zone [u| deg |]
line x =
    Line (Radius earthRadius) (toLL x) 

semicircle :: (Rational, Rational) -> Zone [u| deg |]
semicircle x =
    SemiCircle (Radius earthRadius) (toLL x)

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
        FS.distancePointToPoint [] @?= (TaskDistance $ MkQuantity 0)
    ]

toDistance :: String -> [[ Zone [u| deg |] ]] -> TestTree
toDistance title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "distance ", show x, " = earth radius" ]) $
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

coincident :: String -> [[ Zone [u| deg |] ]] -> TestTree
coincident title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "concident pair of "
                                 , show $ head x
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

touching :: String -> [[ Zone [u| deg |] ]] -> TestTree
touching title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "touching pair of "
                                 , show x
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

disjoint :: String -> [[ Zone [u| deg |] ]] -> TestTree
disjoint title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "disjoint pair of "
                                 , show x 
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

correctPoint :: [ Zone [u| deg |] ] -> TaskDistance -> Bool
correctPoint [] (TaskDistance (MkQuantity d)) = d == 0
correctPoint [_] (TaskDistance (MkQuantity d)) = d == 0
correctPoint xs (TaskDistance (MkQuantity d))
    | all (== head ys) (tail ys) = d == 0
    | otherwise = d > 0
    where
        ys = center <$> xs

correctCenter :: [ Zone [u| deg |] ] -> TaskDistance -> Bool
correctCenter [] (TaskDistance (MkQuantity d)) = d == 0
correctCenter [_] (TaskDistance (MkQuantity d)) = d == 0
correctCenter xs (TaskDistance (MkQuantity d))
    | all (== head ys) (tail ys) = d == 0
    | not $ separatedZones xs = d == 0
    | otherwise = d > 0
    where
        ys = center <$> xs

distancePoint :: ZonesTest -> Bool
distancePoint (ZonesTest xs) =
    correctPoint xs $ FS.distancePointToPoint xs

mm10 :: Tolerance
mm10 = Tolerance $ 10 % 1000

distanceEdge :: ZonesTest -> Bool
distanceEdge (ZonesTest xs) =
    correctCenter xs
    $ centers
    $ FS.distanceEdgeToEdge PathPointToPoint mm10 xs

distanceLess :: ZonesTest -> Bool
distanceLess (ZonesTest xs)
    | length xs < 3 = True
    | otherwise =
        dCenter <= dPoint
        where
            TaskDistance dCenter =
                centers
                $ FS.distanceEdgeToEdge PathPointToPoint mm10 xs

            TaskDistance dPoint = FS.distancePointToPoint xs
