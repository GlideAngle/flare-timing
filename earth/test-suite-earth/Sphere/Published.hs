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

module Sphere.Published (publishedUnits, bedfordUnits, geoSciAuUnits) where

import Prelude hiding (span, min)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure (u, convert)

import Flight.Units ()
import Flight.LatLng.Rational (Epsilon(..))
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Tolerance as T (GetTolerance, dblInverseChecks, ratInverseChecks)
import qualified Published.Bedford as B (inverseProblems, inverseSolutions)
import qualified Published.GeoscienceAustralia as G (inverseProblems, inverseSolutions)
import Flight.Earth.Geodesy (IProb, ISoln)

getTolerance :: (Ord a, Fractional a) => T.GetTolerance a
getTolerance d'
    | d < [u| 100 km |] = convert [u| 425.4 m |]
    | d < [u| 500 km |] = convert [u| 2.495 km |]
    | d < [u| 1000 km |] = [u| 4.06 km |]
    | otherwise = [u| 19.5 km |]
    where
        d = convert d'

dblInverseChecks
    :: [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks =
    T.dblInverseChecks getTolerance $ repeat Dbl.distanceHaversine

ratInverseChecks
    :: [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks =
    T.ratInverseChecks getTolerance $ repeat span
    where
        span = Rat.distanceHaversine e
        e = Epsilon $ 1 % 1000000000000000000

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "with doubles"
        $ dblInverseChecks B.inverseSolutions B.inverseProblems
    , testGroup "with rationals"
        $ ratInverseChecks B.inverseSolutions B.inverseProblems
    ]

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "with doubles"
        $ dblInverseChecks G.inverseSolutions G.inverseProblems
    , testGroup "with rationals"
        $ ratInverseChecks G.inverseSolutions G.inverseProblems
    ]

publishedUnits :: TestTree
publishedUnits =
    testGroup "With published data sets"
    [ geoSciAuUnits
    , bedfordUnits
    ]
