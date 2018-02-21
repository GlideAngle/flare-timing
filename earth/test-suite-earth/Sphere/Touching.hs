{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Touching (Overlay(..), separatedZones, touchingUnits) where

import Prelude hiding (span)
import Data.Ratio ((%))
import Data.List (intersperse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure
    ((-:), (*:), u, convert, negate', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone
    ( HasArea(..), Zone(..), Radius(..)
    , showZoneDMS, fromRationalZone, toRationalZone
    )
import Flight.LatLng.Double (showAngle)
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl
    (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat
    (distanceHaversine)
import qualified Flight.Earth.Sphere.Separated as S (separatedZones)
import Flight.Earth.Sphere (earthRadius)

import Zone (MkZone, QLL, showQ, dotZones, areaZones)

spanD :: SpanLatLng Double
spanD = Dbl.distanceHaversine

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

sepD :: [Zone Double] -> Bool
sepD = S.separatedZones spanD

sepR :: [Zone Rational] -> Bool
sepR = S.separatedZones spanR

touchingUnits :: TestTree
touchingUnits =
    testGroup "Potentially overlapping zones are touching"
    [ testGroup "With doubles"
        [ testGroup "Zones without area are not touching" ((uncurry fD) <$> dotZones)
        , testGroup "Zones with area are touching" ((uncurry gD) <$> areaZones)
        ]
    , testGroup "With rationals"
        [ testGroup "Zones without area are not touching" ((uncurry fR) <$> dotZones)
        , testGroup "Zones with area are touching" ((uncurry gR) <$> areaZones)
        ]
    ]
    where
        fD s =
            zonesTouching
                (s ++ " zones")
                (separatedZones sepD Overlap delta radius)

        gD s =
            zonesTouching
                (s ++ " zones")
                (separatedZones sepD Overlap delta radius)

        fR s =
            zonesTouching
                (s ++ " zones")
                (separatedZones sepR Overlap delta radius)

        gR s =
            zonesTouching
                (s ++ " zones")
                (separatedZones sepR Overlap delta radius)

data Overlay = Overlap | Disjoint deriving (Eq, Show)

separatedZones
    :: (Ord a, Real a)
    => ([Zone a] -> Bool)
    -> Overlay
    -> Quantity a [u| 1 |]
    -> Quantity a [u| 1 |]
    -> String
    -> [[Zone a]]
    -> TestTree
separatedZones sep expected delta' radius' title xs =
    testGroup title (f <$> xs)
    where
        f x =
            testGroup sZone
                [ testGroup sRadius
                    [ HU.testCase sSep
                        $ sep x
                            @?= (expected == Disjoint || withoutArea)
                    ]
                ]
            where
                withoutArea = any (not . hasArea) $ concat xs

                sRadius =
                    mconcat
                        [ "radius="
                        , showAngle radiusDMS
                        , " or "
                        , show radiusRad
                        ]

                sSep =
                    mconcat
                        [ if expected == Disjoint
                            then "separation="
                            else "overlap="
                        , showAngle deltaDMS
                        , " or "
                        , show deltaRad
                        ]

                sZone =
                    mconcat
                     $ intersperse
                         ", "
                         (showZoneDMS . fromRationalZone . toRationalZone <$> x)

        deltaDMS :: Quantity Double [u| dms |]
        deltaDMS = convert $ (realToFrac delta') *: [u| 1 rad |]

        deltaRad :: Quantity Double [u| rad |]
        deltaRad = (realToFrac delta') *: [u| 1 rad |]

        radiusDMS :: Quantity Double [u| dms |]
        radiusDMS = convert $ (realToFrac radius') *: [u| 1 rad |]

        radiusRad :: Quantity Double [u| rad |]
        radiusRad = (realToFrac radius') *: [u| 1 rad |]

type Touching a = String -> [[Zone a]] -> TestTree

delta :: (Real a, Fractional a) => Quantity a [u| 1 |]
delta = fromRational' . MkQuantity $ 1 % 100000000

radius :: (Real a, Fractional a) => Quantity a [u| 1 |]
radius = fromRational' . MkQuantity $ 2 % 1

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    [ ((z, z), (z, pos))
    , ((z, z), (z, neg))
    ]
    where
        z = [u| 0 rad |]

        pos = (radius -: delta) *: [u| 1 rad |]
        neg = negate' (radius -: delta) *: [u| 1 rad |]

distances :: (Real a, Fractional a) => [Radius a [u| m |]]
distances =
    repeat $ Radius earthRadius

zonesTouching
    :: (Enum a, Real a, Fractional a)
    => String
    -> Touching a
    -> MkZone a a
    -> TestTree
zonesTouching s f g =
    testGroup s
    $ zipWith
        (\r (x, y) -> f (showQ x ++ " " ++ showQ y) [[g r x, g r y]])
        distances
        pts
