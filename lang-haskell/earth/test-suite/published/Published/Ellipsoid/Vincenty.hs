{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Published.Ellipsoid.Vincenty (units, unitsR) where

import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..), absDiffDMS, absDiffDMS180)
import Flight.Earth.Ellipsoid (Ellipsoid, wgs84, nad83, bedfordClarke)
import qualified Published.GeoscienceAustralia as G
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import qualified Published.GeodeticSurvey as N
    ( directProblems, directSolutions
    )
import qualified Published.Vincenty1975 as V
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    , ellipsoids
    )
import qualified Published.Bedford1978 as B
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    )
import Tolerance (TestTolerance(..), GetTolerance, AzTolerance)
import qualified Tolerance as T
    ( dblDirectChecks, ratDirectChecks
    , dblInverseChecks, ratInverseChecks
    )
import Flight.Geodesy (DProb, DSoln, IProb, ISoln)
import Ellipsoid.Vincenty.Span (spanD, spanR, azFwdD, azRevD)

units :: TestTree
units =
    testGroup "With published data sets"
    [ geoSciAuUnits
    , ngsUnits
    , vincentyUnits
    , bedfordUnits
    ]

unitsR :: TestTree
unitsR =
    testGroup "With published data sets"
    [ geoSciAuUnitsR
    , ngsUnitsR
    , vincentyUnitsR
    , bedfordUnitsR
    ]

geoSciAuAzTolerance :: AzTolerance
geoSciAuAzTolerance = DMS (0, 0, 0.016664)

vincentyAzTolerance :: AzTolerance
vincentyAzTolerance = DMS (0, 0, 0.016667)

bedfordAzTolerance :: AzTolerance
bedfordAzTolerance = DMS (0, 0, 0.139737)

geoSciAuTolerance :: Fractional a => GetTolerance a
geoSciAuTolerance = const . convert $ [u| 0.5 mm |]

ngsTolerance :: Fractional a => GetTolerance a
ngsTolerance = const . convert $ [u| 0.15 mm |]

vincentyTolerance :: Fractional a => GetTolerance a
vincentyTolerance = const . convert $ [u| 0.8 mm |]

-- From the paper, Vincenty's errors were mm of -0.4, -0.4, -0.7, -0.2 and -0.8.
vincentyInverseDistanceTolerances :: Fractional a => [TestTolerance a]
vincentyInverseDistanceTolerances =
    TestToleranceAmount . convert <$>
        [ [u| 0.404 mm |]
        , [u| 0.387 mm |]
        , [u| 0.703 mm |]
        , [u| 0.197 mm |]
        , [u| 0.787 mm |]
        ]

bedfordTolerance
    :: (Real a, Fractional a)
    => Quantity a [u| m |]
    -> Quantity a [u| km |]
bedfordTolerance d'
    | d < [u| 100 km |] = convert [u| 37 mm |]
    | d < [u| 500 km |] = convert [u| 37 mm |]
    | d < [u| 1000 km |] = convert [u| 37 mm |]
    | otherwise = convert [u| 37 mm |]
    where
        d = convert d'

dblDirectChecks
    :: GetTolerance Double
    -> [Ellipsoid Double]
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
dblDirectChecks tolerance ellipsoid =
    T.dblDirectChecks tolerance (spanD <$> ellipsoid)

ratDirectChecks
    :: GetTolerance Rational
    -> [Ellipsoid Rational]
    -> [DSoln]
    -> [DProb]
    -> [TestTree]
ratDirectChecks tolerance ellipsoid =
    T.ratDirectChecks tolerance (spanR <$> ellipsoid)

dblInverseChecks
    :: [TestTolerance Double]
    -> AzTolerance
    -> [Ellipsoid Double]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecks distTolerances azTolerance ellipsoid =
    T.dblInverseChecks
        absDiffDMS
        absDiffDMS
        distTolerances
        azTolerance
        (spanD <$> ellipsoid)
        (azFwdD <$> ellipsoid)
        (azRevD <$> ellipsoid)

dblInverseChecksDiffAzRev180
    :: [TestTolerance Double]
    -> AzTolerance
    -> [Ellipsoid Double]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
dblInverseChecksDiffAzRev180 distTolerances azTolerance ellipsoid =
    T.dblInverseChecks
        absDiffDMS
        absDiffDMS180
        distTolerances
        azTolerance
        (spanD <$> ellipsoid)
        (azFwdD <$> ellipsoid)
        (azRevD <$> ellipsoid)

ratInverseChecks
    :: GetTolerance Rational
    -> AzTolerance
    -> [Ellipsoid Rational]
    -> [ISoln]
    -> [IProb]
    -> [TestTree]
ratInverseChecks tolerance azTolerance ellipsoid =
    T.ratInverseChecks tolerance azTolerance (spanR <$> ellipsoid)

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecksDiffAzRev180
                (repeat $ TestToleranceLookup geoSciAuTolerance)
                geoSciAuAzTolerance
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
        ]
    ]

geoSciAuUnitsR :: TestTree
geoSciAuUnitsR =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with rationals"
            $ ratInverseChecks
                geoSciAuTolerance
                geoSciAuAzTolerance
                (repeat wgs84)
                G.inverseSolutions
                G.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with rationals"
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
    -- TODO: Investigate why the Geodetic Survey inverse results often disagree
    -- with the azimuth or reverse azimuth flipped 180°.
    {-
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                ngsTolerance
                ngsAzTolerance
                (repeat nad83)
                N.inverseSolutions
                N.inverseProblems
        ]
    -}

    [ testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                ngsTolerance
                (repeat nad83)
                N.directSolutions
                N.directProblems
        ]
    ]

ngsUnitsR :: TestTree
ngsUnitsR =
    testGroup "National Geodetic Survey distances, using Vincenty"
    -- TODO: Investigate why the Geodetic Survey inverse results often disagree
    -- with the azimuth or reverse azimuth flipped 180°.
    {-
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with rationals"
            $ ratInverseChecks
                ngsTolerance
                ngsAzTolerance
                (repeat wgs84)
                N.inverseSolutions
                N.inverseProblems
        ]
    -}

    [ testGroup "Direct Problem of Geodesy"
        [ testGroup "with rationals"
            $ ratDirectChecks
                ngsTolerance
                (repeat wgs84)
                N.directSolutions
                N.directProblems
        ]
    ]

vincentyUnits :: TestTree
vincentyUnits =
    testGroup "Vincenty's distances, from Vincenty 1975 (Rainsford 1955)"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblInverseChecks
                vincentyInverseDistanceTolerances
                vincentyAzTolerance
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
        ]
    ]

vincentyUnitsR :: TestTree
vincentyUnitsR =
    testGroup "Vincenty's distances, from Vincenty 1975 (Rainsford 1955)"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with rationals"
            $ ratInverseChecks
                vincentyTolerance
                vincentyAzTolerance
                V.ellipsoids
                V.inverseSolutions
                V.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with rationals"
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
            $ dblInverseChecksDiffAzRev180
                (repeat $ TestToleranceLookup bedfordTolerance)
                bedfordAzTolerance
                (repeat bedfordClarke)
                B.inverseSolutions
                B.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with doubles"
            $ dblDirectChecks
                bedfordTolerance
                (repeat bedfordClarke)
                B.directSolutions
                B.directProblems
        ]
    ]

bedfordUnitsR :: TestTree
bedfordUnitsR =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "Inverse Problem of Geodesy"
        [ testGroup "with rationals"
            $ ratInverseChecks
                bedfordTolerance
                bedfordAzTolerance
                (repeat bedfordClarke)
                B.inverseSolutions
                B.inverseProblems
        ]

    , testGroup "Direct Problem of Geodesy"
        [ testGroup "with rationals"
            $ ratDirectChecks
                bedfordTolerance
                (repeat bedfordClarke)
                B.directSolutions
                B.directProblems
        ]
    ]
