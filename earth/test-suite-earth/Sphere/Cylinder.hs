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

module Sphere.Cylinder (circumSampleUnits) where

import Prelude hiding (span)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Bearing(..), Radius(..), Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.Cylinder
    (Samples(..), SampleParams(..), Tolerance(..), CircumSample, ZonePoint(..))
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Rational as Rat (circumSample)

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
sampleParams =
    SampleParams
        { spSamples = Samples 100
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

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

cs :: CircumSample Rational
cs = Rat.circumSample

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
