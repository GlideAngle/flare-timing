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
import Flight.LatLng.Rational (Epsilon(..))
import qualified Flight.Earth.Ellipsoid.PointToPoint.Double as Dbl (distanceVincenty)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat (distanceVincenty)
import Flight.Earth.Ellipsoid (wgs84)
import qualified Published.Bedford as B
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import qualified Published.GeoscienceAustralia as G
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import qualified Tolerance as T
    ( GetTolerance
    , dblDirectChecks, ratDirectChecks
    , dblInverseChecks, ratInverseChecks
    )
import Flight.Earth.Geodesy (DProb, DSoln, IProb, ISoln)

getTolerance :: Fractional a => T.GetTolerance a
getTolerance = const . convert $ [u| 0.5 mm |]

dblDirectChecks
    :: [DSoln]
    -> [DProb]
    -> [TestTree]
dblDirectChecks =
    T.dblDirectChecks (Dbl.distanceVincenty wgs84) getTolerance

ratDirectChecks
    :: [DSoln]
    -> [DProb]
    -> [TestTree]
ratDirectChecks =
    T.ratDirectChecks span getTolerance
    where
        span = Rat.distanceVincenty e wgs84
        e = Epsilon $ 1 % 1000000000000000000

dblInverseChecks
    :: [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks =
    T.dblInverseChecks (Dbl.distanceVincenty wgs84) getTolerance

ratInverseChecks
    :: [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks =
    T.ratInverseChecks span getTolerance
    where
        span = Rat.distanceVincenty e wgs84
        e = Epsilon $ 1 % 1000000000000000000

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks B.inverseSolutions B.inverseProblems
        , testGroup "with rationals"
            $ ratInverseChecks B.inverseSolutions B.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks B.directSolutions B.directProblems
        , testGroup "with rationals"
            $ ratDirectChecks B.directSolutions B.directProblems
        ]
    ]

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks G.inverseSolutions G.inverseProblems
        , testGroup "with rationals"
            $ ratInverseChecks G.inverseSolutions G.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks G.directSolutions G.directProblems
        , testGroup "with rationals"
            $ ratDirectChecks G.directSolutions G.directProblems
        ]
    ]

publishedUnits :: TestTree
publishedUnits =
    testGroup "With published data sets"
    [ geoSciAuUnits
    , bedfordUnits
    ]
