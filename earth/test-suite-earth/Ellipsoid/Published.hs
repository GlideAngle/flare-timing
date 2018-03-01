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

module Ellipsoid.Published (publishedUnits, bedfordUnits, geoSciAuUnits) where

import Prelude hiding (span, min)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure (u, convert)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (TaskDistance(..))
import qualified Flight.Earth.Ellipsoid.PointToPoint.Double as Dbl (distanceVincenty)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat (distanceVincenty)
import Flight.Earth.Ellipsoid (wgs84)
import qualified Bedford as B (points, solutions)
import qualified GeoscienceAustralia as G (points, solutions)
import qualified Tolerance as T (GetTolerance, dblChecks, ratChecks)

getTolerance :: Fractional a => T.GetTolerance a
getTolerance = const . convert $ [u| 0.5 mm |]

dblChecks
    :: [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))]
    -> [TestTree]
dblChecks =
    T.dblChecks (Dbl.distanceVincenty wgs84) getTolerance

ratChecks
    :: [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))]
    -> [TestTree]
ratChecks =
    T.ratChecks span getTolerance
    where
        span = Rat.distanceVincenty e wgs84
        e = Epsilon $ 1 % 1000000000000000000

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "with doubles" $ dblChecks B.solutions B.points
    , testGroup "with rationals" $ ratChecks B.solutions B.points
    ]

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "with doubles" $ dblChecks G.solutions G.points
    , testGroup "with rationals" $ ratChecks G.solutions G.points
    ]

publishedUnits :: TestTree
publishedUnits =
    testGroup "With published data sets"
    [ geoSciAuUnits
    , bedfordUnits
    ]
