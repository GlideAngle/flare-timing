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
module Published.GeoscienceAustralia
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    ) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import Flight.Geodesy
    ( GeodesyProblems(..)
    , InverseProblem(..), InverseSolution(..)
    , DProb, DSoln
    , IProb, ISoln
    )

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
    [InverseProblem flindersPeak buninyong]
    where
        flindersPeak = (DMS (-37, 57, 03.7203), DMS (144, 25, 29.5244))
        buninyong = (DMS (-37, 39, 10.1561), DMS (143, 55, 35.3839))

inverseSolutions :: [ISoln]
inverseSolutions =
    [ InverseSolution
        { s = TaskDistance [u| 54972.271 m |]
        , α₁ = DMS (306, 52, 05.373)
        , α₂ = Just $ DMS (127, 10, 25.070)
        }
    ]
