{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flat.Bedford (bedfordUnits) where

import Prelude hiding (min)
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import qualified Flight.Earth.Flat.PointToPoint.Double as Dbl (distanceEuclidean)
import qualified Flight.Earth.Flat.PointToPoint.Rational as Rat (distanceEuclidean)
import qualified Tolerance as T (dblInverseChecks, ratInverseChecks)
import Published.Bedford (inverseProblems, inverseSolutions)
import Flight.Earth.Geodesy (IProb, ISoln)

getTolerance
    :: (Real a, Fractional a)
    => Quantity a [u| m |]
    -> Quantity a [u| km |]
getTolerance d'
    | d < [u| 100 km |] = convert [u| 376 km |]
    | d < [u| 500 km |] = convert [u| 431 km |]
    | d < [u| 1000 km |] = [u| 658 km |]
    | otherwise = [u| 4470 km |]
    where
        d = convert d'

dblInverseChecks
    :: [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks =
    T.dblInverseChecks Dbl.distanceEuclidean getTolerance

ratInverseChecks
    :: [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks =
    T.ratInverseChecks Rat.distanceEuclidean getTolerance

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "with doubles"
        $ dblInverseChecks inverseSolutions inverseProblems
    , testGroup "with rationals"
        $ ratInverseChecks inverseSolutions inverseProblems
    ]
