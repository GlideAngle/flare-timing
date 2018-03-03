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
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (SpanLatLng)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Double as Dbl (distanceVincenty)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat (distanceVincenty)
import Flight.Earth.Ellipsoid (wgs84)
import qualified Published.GeoscienceAustralia as G
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import qualified Published.Vincenty as V
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import qualified Published.Bedford as B
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

bedfordDirectTolerance
    :: (Real a, Fractional a)
    => Quantity a [u| m |]
    -> Quantity a [u| km |]
bedfordDirectTolerance d'
    | d < [u| 100 km |] = convert [u| 5 m |]
    | d < [u| 500 km |] = convert [u| 29 m |]
    | d < [u| 1000 km |] = convert [u| 47 m |]
    | otherwise = convert [u| 208 m |]
    where
        d = convert d'

e :: Epsilon
e = Epsilon $ 1 % 1000000000000000000

spanR :: SpanLatLng Rational
spanR = Rat.distanceVincenty e wgs84

dblDirectChecks
    :: T.GetTolerance Double
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
dblDirectChecks tolerance =
    T.dblDirectChecks (Dbl.distanceVincenty wgs84) tolerance

ratDirectChecks
    :: T.GetTolerance Rational
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
ratDirectChecks tolerance =
    T.ratDirectChecks spanR tolerance

dblInverseChecks
    :: T.GetTolerance Double
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks tolerance =
    T.dblInverseChecks (Dbl.distanceVincenty wgs84) tolerance

ratInverseChecks
    :: T.GetTolerance Rational
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks tolerance =
    T.ratInverseChecks spanR tolerance

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks getTolerance G.inverseSolutions G.inverseProblems
        , testGroup "with rationals"
            $ ratInverseChecks getTolerance G.inverseSolutions G.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks getTolerance G.directSolutions G.directProblems
        , testGroup "with rationals"
            $ ratDirectChecks getTolerance G.directSolutions G.directProblems
        ]
    ]

vincentyUnits :: TestTree
vincentyUnits =
    testGroup "Vincenty's distances, from Rainsford 1955"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks getTolerance V.inverseSolutions V.inverseProblems
        , testGroup "with rationals"
            $ ratInverseChecks getTolerance V.inverseSolutions V.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks getTolerance V.directSolutions V.directProblems
        , testGroup "with rationals"
            $ ratDirectChecks getTolerance V.directSolutions V.directProblems
        ]
    ]

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks getTolerance B.inverseSolutions B.inverseProblems
        , testGroup "with rationals"
            $ ratInverseChecks getTolerance B.inverseSolutions B.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                bedfordDirectTolerance B.directSolutions B.directProblems
        , testGroup "with rationals"
            $ ratDirectChecks
                bedfordDirectTolerance B.directSolutions B.directProblems
        ]
    ]

publishedUnits :: TestTree
publishedUnits =
    testGroup "With published data sets"
    [ geoSciAuUnits
    , vincentyUnits
    , bedfordUnits
    ]
