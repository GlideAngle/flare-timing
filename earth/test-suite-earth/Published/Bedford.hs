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
{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- | Test data from ...
-- 
-- Bedford Institute of Oceanography
-- Evaluation Direct and Inverse Geodetic Algorithms
-- Paul Delorme, September 1978.
module Published.Bedford
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    ) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import qualified Geodesy as G
    (DirectProblem(..), DirectSolution(..))
import Geodesy
    ( GeodesyProblems(..)
    , InverseProblem(..), InverseSolution(..)
    , DProb, DSoln
    , IProb, ISoln
    )

-- ghc: panic! (the 'impossible' happened)
--  (GHC version 8.2.2 for x86_64-apple-darwin):
-- translateConPatVec: lookup
--
-- Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
updateDistance
    :: TaskDistance Double
    -> (DProb, DSoln)
    -> (DProb, DSoln)
updateDistance _ (G.DirectProblem{x, α₁}, soln) =
    (prob', soln)
    where
        prob' =
            G.DirectProblem{x = x, α₁ = α₁}

directPairs :: [(DProb, DSoln)]
directPairs =
    catMaybes $
    [ updateDistance d <$> direct ip is
    | ip <- inverseProblems
    | is <- inverseSolutions
    | d <-TaskDistance <$> directDistances
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
    [ (((10,  0,  0.0), (-18,  0,  0.0)), ((10, 43, 39.078), (-18,  0,  0.0)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((40, 43, 28.790), (-18,  0,  0.0)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 43, 16.379), (-18,  0,  0.0)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), ((10, 30, 50.497), (-17, 28, 48.777)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((40, 30, 37.757), (-17, 19, 43.280)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 30, 12.925), (-16, 28, 22.844)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 59, 57.087), (-17, 15, 57.926)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 59, 46.211), (-17,  3, 27.942)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((69, 59, 15.149), (-15, 53, 37.449)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 38,  8.260), (-17, 21, 54.407)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 31, 54.913), (-18, 43,  1.027)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 42, 35.533), (-18, 22, 43.683)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), ((14, 21, 52.456), (-18,  0,  0.0)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((44, 20, 47.740), (-18,  0,  0.0)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((74, 19, 35.289), (-18,  0,  0.0)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), ((13,  4, 12.564), (-14, 51, 13.283)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((43,  0,  0.556), (-13, 48, 49.111)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((72, 47, 48.242), ( -7, 36, 58.487)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 58, 15.192), (-13, 35, 48.467)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 51, 44.295), (-12, 21, 14.090)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((69, 33, 22.562), ( -5, 32,  1.822)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), ((17, 16, 24.286), (-18,  0,  0.0)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((47, 14, 32.867), (-18,  0,  0.0)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((77, 12, 35.253), (-18,  0,  0.0)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), ((15,  5, 43.367), (-12, 42, 50.044)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((44, 54, 28.506), (-10, 47, 43.884)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((74, 17,  5.184), (  1,  6, 51.561)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 55,  9.138), (-10, 39, 43.554)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 37,  6.613), ( -8, 36, 43.277)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((68, 47, 25.009), (  2, 17, 23.583)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), ((53, 32,  0.497), (-18,  0,  0.0)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((83, 20,  1.540), (-18,  0,  0.0)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((66, 45, 22.460), (162,  0,  0.0)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), ((37, 18, 49.295), ( 19, 34,  7.117)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((57,  6,  0.851), ( 45,  8, 40.841)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((58, 13,  5.486), ( 95,  2, 29.439)))

    , (((10,  0,  0.0), (-18,  0,  0.0)), (( 7, 14,  5.521), ( 25, 48, 13.908)))
    , (((40,  0,  0.0), (-18,  0,  0.0)), ((27, 49, 42.130), ( 32, 54, 13.184)))
    , (((70,  0,  0.0), (-18,  0,  0.0)), ((43,  7, 36.475), ( 52,  1,  0.626)))
    ]

inverseSolutions :: [ISoln]
inverseSolutions =
    [ InverseSolution
        { s = d
        , α₁ = azX
        , α₂ = azY
        }
    | d <- TaskDistance . convert <$> inverseDistances
    | azX <- xAzimuths
    | azY <- yAzimuths
    ]

inverseDistances :: [Quantity Double [u| km |]]
inverseDistances =
    [ [u| 80.471341 km |]
    , [u| 80.467842 km |]
    , [u| 80.463616 km |]

    , [u| 80.468422 km |]
    , [u| 80.466106 km |]
    , [u| 80.463284 km |]

    , [u| 80.465497 km |]
    , [u| 80.464363 km |]
    , [u| 80.462951 km |]

    , [u| 80.466994 km |]
    , [u| 80.4659 km |]
    , [u| 80.463589 km |]

    , [u| 482.827311 km |]
    , [u| 482.805398 km |]
    , [u| 482.780699 km |]

    , [u| 482.810039 km |]
    , [u| 482.795399 km |]
    , [u| 482.778968 km |]

    , [u| 482.793074 km |]
    , [u| 482.786227 km |]
    , [u| 482.777777 km |]

    , [u| 804.711122 km |]
    , [u| 804.673374 km |]
    , [u| 804.633279 km |]

    , [u| 804.682723 km |]
    , [u| 804.657459 km |]
    , [u| 804.630834 km |]

    , [u| 804.655123 km |]
    , [u| 804.643847 km |]
    , [u| 804.629907 km |]

    , [u| 4828.136258 km |]
    , [u| 4827.891819 km |]
    , [u| 4827.781145 km |]

    , [u| 4828.022935 km |]
    , [u| 4827.861414 km |]
    , [u| 4827.797208 km |]

    , [u| 4827.933527 km |]
    , [u| 4827.899946 km |]
    , [u| 4827.858337 km |]
    ]

xAzimuths :: [DMS]
xAzimuths =
    replicate 3 z
    ++ replicate 3 (deg 45)
    ++ replicate 3 (deg 90)
    ++ [deg 120, deg 230, deg 350]
    ++ replicate 3 z
    ++ replicate 3 (deg 45)
    ++ replicate 3 (deg 90)
    ++ replicate 3 z
    ++ replicate 3 (deg 45)
    ++ replicate 3 (deg 90)
    ++ replicate 3 z
    ++ replicate 3 (deg 45)
    ++ replicate 3 (deg 90)
    where
        deg d = DMS (d, 0, 0)
        z = deg 0

yAzimuths :: [Maybe DMS]
yAzimuths =
    Just . DMS <$>
    xs ++
    [ (225,  5, 33.202)
    , (225, 26,  1.695)
    , (226,  7, 13.935)

    , (270,  7, 38.779)
    , (270, 36, 20.315)
    , (271, 58, 45.080)

    , (300,  6, 29.736)
    , ( 49, 32, 29.011)
    , (169, 38, 35.667)
    ]
    ++ xs ++
    [ (225, 37, 46.346)
    , (227, 46, 32.221)
    , (234, 50, 49.050)

    , (270, 45, 49.945)
    , (273, 37, 32.768)
    , (281, 42, 12.088)
    ]
    ++ xs ++
    [ (226,  9,  1.224)
    , (229, 52, 15.525)
    , (243, 13, 18.356)

    , (271, 16, 14.933)
    , (276,  1,  6.634)
    , (289,  1,  2.923)

    , (180,  0,  0.0)
    , (180,  0,  0.0)
    , (  0,  0,  0.0)

    , (240, 59, 37.859)
    , (274, 57, 29.108)
    , (332, 38, 58.143)

    , (276, 53, 56.143)
    , (299, 54, 41.259)
    , (332,  0, 43.685)
    ]
    where
        xs = replicate 3 (180,  0,  0.0)

directDistances :: [Quantity Double [u| m |]]
directDistances =
    replicate 3 [u| 80466.478 m |]
    ++
    [ [u| 80466.477 m |]
    ]
    ++ replicate 2 [u| 80466.478 m |]
    ++
    [ [u| 80466616 m |]
    ]
    ++
    [ [u| 80466.476 m |] 
    , [u| 80466.477 m |] 
    ]
    ++ replicate 4 [u| 80466.478 m |]
    ++ replicate 9 [u| 482798.868 m |]
    ++ replicate 9 [u| 804664.780 m |]
    ++ replicate 9 [u| 4827988.683 m |]
