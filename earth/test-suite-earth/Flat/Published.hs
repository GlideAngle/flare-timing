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

module Flat.Published
    ( publishedUnits
    , geoSciAuUnits
    , vincentyUnits
    , bedfordUnits
    ) where

import Prelude hiding (span, min)
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
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
import Flat.Span (spanD, spanR)

-- | TODO: Find out why we're out 430 km over 55 km on a flat Earth.
geoSciAuTolerance :: Fractional a => T.GetTolerance a
geoSciAuTolerance = const . convert $ [u| 431 km |]

-- | TODO: Find out why we're out 20000 km for Vincenty's tests on a flat Earth.
vincentyTolerance
    :: (Real a, Fractional a)
    => Quantity a [u| m |]
    -> Quantity a [u| km |]
vincentyTolerance d'
    | d < [u| 5000 km |] = convert [u| 2750 km |]
    | d < [u| 10000 km |] = convert [u| 4500 km |]
    | otherwise = convert [u| 19200 km |]
    where
        d = convert d'

-- | TODO: Find out why we're out 4500 km for Bedford tests on a flat Earth.
bedfordTolerance
    :: (Real a, Fractional a)
    => Quantity a [u| m |]
    -> Quantity a [u| km |]
bedfordTolerance d'
    | d < [u| 100 km |] = convert [u| 376 km |]
    | d < [u| 1000 km |] = convert [u| 658 km |]
    | otherwise = convert [u| 4500 km |]
    where
        d = convert d'

dblDirectChecks
    :: T.GetTolerance Double
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
dblDirectChecks tolerance =
    T.dblDirectChecks tolerance (repeat spanD)

ratDirectChecks
    :: T.GetTolerance Rational
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
ratDirectChecks tolerance =
    T.ratDirectChecks tolerance (repeat spanR)

dblInverseChecks
    :: T.GetTolerance Double
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks tolerance =
    T.dblInverseChecks tolerance (repeat spanD)

ratInverseChecks
    :: T.GetTolerance Rational
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks tolerance =
    T.ratInverseChecks tolerance (repeat spanR)

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                geoSciAuTolerance
                G.inverseSolutions
                G.inverseProblems

        , testGroup "with rationals"
            $ ratInverseChecks
                geoSciAuTolerance
                G.inverseSolutions
                G.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                geoSciAuTolerance
                G.directSolutions
                G.directProblems
        , testGroup "with rationals"
            $ ratDirectChecks
                geoSciAuTolerance
                G.directSolutions
                G.directProblems
        ]
    ]

vincentyUnits :: TestTree
vincentyUnits =
    testGroup "Vincenty's distances, from Rainsford 1955"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                vincentyTolerance
                V.inverseSolutions
                V.inverseProblems

        , testGroup "with rationals"
            $ ratInverseChecks
                vincentyTolerance
                V.inverseSolutions
                V.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                vincentyTolerance
                V.directSolutions
                V.directProblems

        , testGroup "with rationals"
            $ ratDirectChecks
                vincentyTolerance
                V.directSolutions
                V.directProblems
        ]
    ]

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                bedfordTolerance
                B.inverseSolutions
                B.inverseProblems

        , testGroup "with rationals"
            $ ratInverseChecks
                bedfordTolerance
                B.inverseSolutions
                B.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                bedfordTolerance
                B.directSolutions
                B.directProblems

        , testGroup "with rationals"
            $ ratDirectChecks
                bedfordTolerance
                B.directSolutions
                B.directProblems
        ]
    ]

publishedUnits :: TestTree
publishedUnits =
    testGroup "With published data sets"
    [ geoSciAuUnits
    , vincentyUnits
    , bedfordUnits
    ]
