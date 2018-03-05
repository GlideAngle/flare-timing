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

module Ellipsoid.Published
    ( publishedUnits
    , geoSciAuUnits
    , vincentyUnits
    , bedfordUnits
    ) where

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
import Flight.Earth.Ellipsoid (Ellipsoid, wgs84, clarke)
import qualified Published.GeoscienceAustralia as G
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import qualified Published.GeodeticSurvey as N
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import qualified Published.Vincenty as V
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    , ellipsoids
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

geoSciAuTolerance :: Fractional a => T.GetTolerance a
geoSciAuTolerance = const . convert $ [u| 0.5 mm |]

ngsTolerance :: Fractional a => T.GetTolerance a
ngsTolerance = const . convert $ [u| 0.15 mm |]

vincentyTolerance :: Fractional a => T.GetTolerance a
vincentyTolerance = const . convert $ [u| 0.8 mm |]

bedfordTolerance
    :: (Real a, Fractional a)
    => Quantity a [u| m |]
    -> Quantity a [u| km |]
bedfordTolerance d'
    | d < [u| 100 km |] = convert [u| 37 mm |]
    | d < [u| 500 km |] = convert [u| 12 mm |]
    | d < [u| 1000 km |] = convert [u| 15 mm |]
    | otherwise = convert [u| 16 mm |]
    where
        d = convert d'

e :: Epsilon
e = Epsilon $ 1 % 1000000000000000000

spanD :: Ellipsoid Double -> SpanLatLng Double
spanD = Dbl.distanceVincenty

spanR :: Ellipsoid Rational -> SpanLatLng Rational
spanR = Rat.distanceVincenty e

dblDirectChecks
    :: T.GetTolerance Double
    -> [Ellipsoid Double]
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
dblDirectChecks tolerance ellipsoid =
    T.dblDirectChecks tolerance (spanD <$> ellipsoid)

ratDirectChecks
    :: T.GetTolerance Rational
    -> [Ellipsoid Rational]
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
ratDirectChecks tolerance ellipsoid =
    T.ratDirectChecks tolerance (spanR <$> ellipsoid)

dblInverseChecks
    :: T.GetTolerance Double
    -> [Ellipsoid Double]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks tolerance ellipsoid =
    T.dblInverseChecks tolerance (spanD <$> ellipsoid)

ratInverseChecks
    :: T.GetTolerance Rational
    -> [Ellipsoid Rational]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks tolerance ellipsoid =
    T.ratInverseChecks tolerance (spanR <$> ellipsoid)

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                geoSciAuTolerance
                (repeat wgs84)
                G.inverseSolutions
                G.inverseProblems

        , testGroup "with rationals"
            $ ratInverseChecks
                geoSciAuTolerance
                (repeat wgs84)
                G.inverseSolutions
                G.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                geoSciAuTolerance
                (repeat wgs84)
                G.directSolutions
                G.directProblems

        , testGroup "with rationals"
            $ ratDirectChecks
                geoSciAuTolerance
                (repeat wgs84)
                G.directSolutions
                G.directProblems
        ]
    ]

ngsUnits :: TestTree
ngsUnits =
    testGroup "National Geodetic Survey distances, using Vincenty"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                ngsTolerance
                (repeat wgs84)
                N.inverseSolutions
                N.inverseProblems

        , testGroup "with rationals"
            $ ratInverseChecks
                ngsTolerance
                (repeat wgs84)
                N.inverseSolutions
                N.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                ngsTolerance
                (repeat wgs84)
                N.directSolutions
                N.directProblems

        , testGroup "with rationals"
            $ ratDirectChecks
                ngsTolerance
                (repeat wgs84)
                N.directSolutions
                N.directProblems
        ]
    ]

vincentyUnits :: TestTree
vincentyUnits =
    testGroup "Vincenty's distances, from Rainsford 1955"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                vincentyTolerance
                V.ellipsoids
                V.inverseSolutions
                V.inverseProblems

        , testGroup "with rationals"
            $ ratInverseChecks
                vincentyTolerance
                V.ellipsoids
                V.inverseSolutions
                V.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                vincentyTolerance
                V.ellipsoids
                V.directSolutions
                V.directProblems

        , testGroup "with rationals"
            $ ratDirectChecks
                vincentyTolerance
                V.ellipsoids
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
                (repeat clarke)
                B.inverseSolutions
                B.inverseProblems

        , testGroup "with rationals"
            $ ratInverseChecks
                bedfordTolerance
                (repeat clarke)
                B.inverseSolutions
                B.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                bedfordTolerance
                (repeat clarke)
                B.directSolutions
                B.directProblems

        , testGroup "with rationals"
            $ ratDirectChecks
                bedfordTolerance
                (repeat clarke)
                B.directSolutions
                B.directProblems
        ]
    ]

publishedUnits :: TestTree
publishedUnits =
    testGroup "With published data sets"
    [ geoSciAuUnits
    , ngsUnits
    , vincentyUnits
    , bedfordUnits
    ]
