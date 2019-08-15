-- | Test data from ...
-- 
-- Direct and Inverse Solutions of Geodesics on the Ellipsoid with Applications
-- of Nested Equations
-- Survey Review XXII, 176
-- T. Vincenty, April 1975.
module Published.Vincenty1975
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    , ellipsoids
    ) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import Flight.Earth.Ellipsoid (Ellipsoid, bessel, hayford)
import Flight.Geodesy
    ( GeodesyProblems(..)
    , InverseProblem(..), InverseSolution(..)
    , DProb, DSoln
    , IProb, ISoln
    )

ellipsoids :: Fractional a => [Ellipsoid a]
ellipsoids =
    bessel : repeat hayford

directPairs :: [(DProb, DSoln)]
directPairs =
    catMaybes
    [ direct ip is
    | ip <- inverseProblems
    | is <- inverseSolutions
    ]

directProblems :: [DProb]
directProblems = fst <$> directPairs

directSolutions :: [DSoln]
directSolutions = snd <$> directPairs

inverseProblems :: [IProb]
inverseProblems =
    (\((xLat, xLng), (yLat, yLng)) ->
        InverseProblem
            { x = (DMS xLat, DMS xLng)
            , y = (DMS yLat, DMS yLng)
            })
    <$>
    [
        (
            ( ( 55, 45,  0.0)
            , (  0,  0,  0.0)
            )
        ,
            ( (-33, 26,  0.0)
            , (108, 13,  0.0)
            )
        )
    ,
        (
            ( ( 37, 19, 54.95367)
            , (  0,  0,  0.0)
            )
        ,
            ( ( 26,  7, 42.83946)
            , ( 41, 28, 35.50729)
            )
        )
    ,
        (
            ( ( 35, 16, 11.24862)
            , (  0,  0,  0.0)
            )
        ,
            ( ( 67, 22, 14.77638)
            , (137, 47, 28.31435)
            )
        )
    ,
        (
            ( (  1,  0,  0.0)
            , (  0,  0,  0.0)
            )
        ,
            ( ( 0, -59, 53.83076)
            , (179, 17, 48.02997)
            )
        )
    ,
        (
            ( (  1,  0,  0.0)
            , (  0,  0,  0.0)
            )
        ,
            ( (  1,  1, 15.18952)
            , (179, 46, 17.84244)
            )
        )
    ]

inverseSolutions :: [ISoln]
inverseSolutions =
    [ InverseSolution
        { s = d
        , α₁ = azX
        , α₂ = azY
        }
    | d <- TaskDistance . convert <$> distances
    | azX <- xAzimuths
    | azY <- yAzimuths
    ]

distances :: [Quantity Double [u| km |]]
distances =
    convert <$>
    [ [u| 14110526.170 m |]
    , [u|  4085966.703 m |]
    , [u|  8084823.839 m |]
    , [u| 19960000.000 m |]
    , [u| 19780006.558 m |]
    ]

xAzimuths :: [DMS]
xAzimuths =
    DMS <$>
    [ ( 96, 36,  8.79960)
    , ( 95, 27, 59.63089)
    , ( 15, 44, 23.74850)
    , ( 89,  0,  0.0)
    , (  4, 59, 59.99995)
    ]

yAzimuths :: [Maybe DMS]
yAzimuths =
    Just . DMS <$>
    [ (137, 52, 22.01454)
    , (118,  5, 58.96161)
    , (144, 55, 39.92147)
    , ( 91,  0,  6.11733)
    , (174, 59, 59.88481)
    ]
