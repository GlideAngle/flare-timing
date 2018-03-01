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

-- | Test data from ...
--
-- Geoscience Australia
-- Geodetic Calculations - Vincenty's Formulae, Inverse Method
-- SEE: http://www.ga.gov.au/geodesy/datums/vincenty_inverse.jsp
-- SEE: http://www.ga.gov.au/scientific-topics/positioning-navigation/geodesy/geodetic-techniques/calculation-methods
-- SEE: http://www.ga.gov.au/__data/assets/file/0019/11377/Vincentys-formulae-to-calculate-distance-and-bearing-from-latitude-and-longitude.xls
--
-- Using the spreadsheet with WGS84
-- A = 6,378,137.00
-- 1/f = 298.257223563
--
-- Flinders Peak
-- Lat/Lng: -37°57'03.7203"/144°25'29.5244"
--
-- Buninyong
-- Lat/Lng: -37°39'10.1561"/143°55'35.3839"
--
-- Distance: 54972.271m
-- Azimuth 1-2: 306°52'05.373"
-- Azimuth 2-1: 127°10'25.070"
module GeoscienceAustralia (GetTolerance, points, solutions, dblChecks, ratChecks) where

import Prelude hiding (span, min)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit as HU (testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.LatLng (fromDMS)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Zone (toRationalLatLng)
import Tolerance (diff, showTolerance)

points :: [((DMS, DMS), (DMS, DMS))]
points =
    [(flindersPeak, buninyong)]
    where
        flindersPeak = (DMS (-37, 57, 03.7203), DMS (144, 25, 29.5244))
        buninyong = (DMS (-37, 39, 10.1561), DMS (143, 55, 35.3839))

solutions :: [TaskDistance Double]
solutions =
    [TaskDistance [u| 54972.271 m |]]

type GetTolerance a = Quantity a [u| m |] -> Quantity a [u| km |]

dblChecks
    :: SpanLatLng Double
    -> GetTolerance Double
    -> [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))] -> [TestTree]
dblChecks span getTolerance =
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

        found x y = span (fromDMS x) (fromDMS y)

ratChecks
    :: SpanLatLng Rational
    -> GetTolerance Rational
    -> [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))]
    -> [TestTree]
ratChecks span getTolerance =
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
        found x y = span (fromDMS' x) (fromDMS' y)
        fromDMS' = toRationalLatLng . fromDMS
