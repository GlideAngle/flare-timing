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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Sphere.Bedford (bedfordUnits) where

import Prelude hiding (min)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (TaskDistance(..))
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import DegMinSec (DMS(..), toDeg)
import Bedford (points, solutions, diff, showTolerance)

getTolerance
    :: (Real a, Fractional a)
    => Quantity a [u| m |]
    -> Quantity a [u| km |]
getTolerance d'
    | d < [u| 100 km |] = convert [u| 425.4 m |]
    | d < [u| 500 km |] = convert [u| 2.495 km |]
    | d < [u| 1000 km |] = [u| 4.06 km |]
    | otherwise = [u| 19.5 km |]
    where
        d = convert d'

dblChecks :: [TaskDistance Double] -> [((DMS, DMS), (DMS, DMS))] -> [TestTree]
dblChecks =
    zipWith f
    where
        f expected (x, y) =
            HU.testCase
                ( show x
                ++ " to "
                ++ show y
                ++ " = "
                ++ show expected
                ++ " ± "
                ++ showTolerance tolerance'
                )
            $ diff (found x y) expected
            @?<= (TaskDistance tolerance')
            where
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) expected

        found x y = Dbl.distanceHaversine (toLL x) (toLL y)

        toLL :: (DMS, DMS) -> LatLng Double [u| rad |]
        toLL (lat, lng) =
            LatLng (Lat lat'', Lng lng'')
                where
                    lat' :: Quantity Double [u| deg |]
                    lat' = MkQuantity . toDeg $ lat

                    lng' :: Quantity Double [u| deg |]
                    lng' = MkQuantity . toDeg $ lng

                    lat'' = convert lat' :: Quantity _ [u| rad |]
                    lng'' = convert lng' :: Quantity _ [u| rad |]

ratChecks :: [TaskDistance Double] -> [((DMS, DMS), (DMS, DMS))] -> [TestTree]
ratChecks =
    zipWith f
    where
        f (TaskDistance d) (x, y) =
            HU.testCase
                ( show x
                ++ " to "
                ++ show y
                ++ " = "
                ++ show expected'
                ++ " ± "
                ++ showTolerance tolerance'
                )
            $ diff (found x y) expected'
            @?<= (TaskDistance tolerance')
            where
                expected' = expected d
                tolerance' =
                    convert . getTolerance
                    $ (\(TaskDistance q) -> q) expected'

        expected d = TaskDistance $ toRational' d
        found x y = Rat.distanceHaversine e (toLL x) (toLL y)

        e = Epsilon $ 1 % 1000000000000000000
        toLL :: (DMS, DMS) -> LatLng Rational [u| rad |]
        toLL (lat, lng) =
            LatLng (Lat lat'', Lng lng'')
                where
                    lat' :: Quantity Rational [u| deg |]
                    lat' = toRational' . MkQuantity . toDeg $ lat

                    lng' :: Quantity Rational [u| deg |]
                    lng' = toRational' . MkQuantity . toDeg $ lng

                    lat'' = convert lat' :: Quantity _ [u| rad |]
                    lng'' = convert lng' :: Quantity _ [u| rad |]

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "with doubles" $ dblChecks solutions points
    , testGroup "with rationals" $ ratChecks solutions points
    ]
