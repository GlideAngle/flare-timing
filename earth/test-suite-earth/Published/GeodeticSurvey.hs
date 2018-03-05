{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Test data generated using forward and inverse programs.
--
-- National Geodetic Survey
-- SEE: https://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/
--
-- >>> inverse
--     Program Inverse  -  Version 3.0
-- 
--    Ellipsoid options :
-- 
--    1) GRS80 / WGS84  (NAD83)
--    2) Clarke 1866    (NAD27)
--    3) Any other ellipsoid
-- 
--    Enter choice :
-- 2
-- 
--    Enter First Station
--                   (Separate D,M,S by blanks or commas)
--  hDD MM SS.sssss  Latitude :        (h default = N )
-- 10 0 0
--  hDDD MM SS.sssss Longitude :       (h default = W )
-- -18 0 0
-- 
--    Enter Second Station
--                   (Separate D,M,S by blanks or commas)
--  hDD MM SS.sssss  Latitude :        (h default = N )
-- 10 43 39.078
--  hDDD MM SS.sssss Longitude :       (h default = W )
-- -18 0 0
-- 
--   Ellipsoid : Clarke 1866    (NAD27)
--   Equatorial axis,    a   =    6378206.4000
--   Polar axis,         b   =    6356583.8000
--   Inverse flattening, 1/f =  294.97869821380
-- 
--    First  Station :
--    ----------------
--     LAT =  10  0  0.00000 North
--     LON =  18  0  0.00000 West
-- 
--    Second Station :
--    ----------------
--     LAT =  10 43 39.07800 North
--     LON =  18  0  0.00000 West
-- 
--   Forward azimuth        FAZ =   0  0  0.0000 From North
--   Back azimuth           BAZ = 180  0  0.0000 From North
--   Ellipsoidal distance     S =     80466.4897 m
--
-- >>> forward
--    Program Forward  -  Version 2.0
-- 
--    Ellipsoid options :
-- 
--    1) GRS80 / WGS84  (NAD83)
--    2) Clarke 1866    (NAD27)
--    3) Any other ellipsoid
-- 
--    Enter choice :
-- 1
-- 
--    Enter First Station
--                   (Separate D,M,S by blanks or commas)
--  hDD MM SS.sssss  Latitude :        (h default = N )
-- 45
--  hDDD MM SS.sssss Longitude :       (h default = W )
-- 180
--  DDD MM SS.sss    Forward Azimuth :     (from north)
-- 90
--  DDDDDD.dddd      Ellipsoidal Distance : (in meters)
-- 40
-- 
--   Ellipsoid : GRS80 / WGS84  (NAD83)
--   Equatorial axis,    a   =    6378137.0000
--   Polar axis,         b   =    6356752.3141
--   Inverse flattening, 1/f =  298.25722210088
-- 
--    First  Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON = 180  0  0.00000 West
-- 
--    Second Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON = 179 59 58.17367 West
-- 
--   Forward azimuth        FAZ =  90  0  0.0000 From North
--   Back azimuth           BAZ = 270  0  1.2914 From North
--   Ellipsoidal distance     S =        40.0000 m
module Published.GeodeticSurvey
    ( directProblems, directSolutions
    , inverseProblems, inverseSolutions
    ) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import Flight.Earth.Geodesy
    ( GeodesyProblems(..)
    , DirectProblem(..), DirectSolution(..)
    , DProb, DSoln
    , IProb, ISoln
    )

inverseProblems :: [IProb]
inverseProblems = fst <$> inversePairs

inverseSolutions :: [ISoln]
inverseSolutions = snd <$> inversePairs

directProblems :: [DProb]
directProblems = fst <$> directPairs

directSolutions :: [DSoln]
directSolutions = snd <$> directPairs

inversePairs :: [(IProb, ISoln)]
inversePairs =
    catMaybes $
    [ uncurry inverse $ dp
    | dp <- directPairs
    ]

directPairs :: [(DProb, DSoln)]
directPairs =
    directLat45Lng180S40

-- | With a common first station and ellipsoidal distance of;
--
--    First  Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON = 180  0  0.00000 West
--
--   Ellipsoidal distance     S =        40.0000 m
directLat45Lng180S40 :: [(DProb, DSoln)]
directLat45Lng180S40 =
    [
--    Second Station :
--    ----------------
--     LAT =  45  0  1.29576 North
--     LON = 180  0  0.00000 West
-- 
--   Forward azimuth        FAZ =   0  0  0.0000 From North
--   Back azimuth           BAZ = 180  0  0.0000 From North
        ( DirectProblem x (DMS (0, 0, 0)) d
        , DirectSolution
            (DMS (45, 0, 1.29576), DMS (180, 0, 0)) 
            (Just (DMS (180, 0, 0)))
        )
    ,

--    Second Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON = 179 59 58.17367 West
-- 
--   Forward azimuth        FAZ =  90  0  0.0000 From North
--   Back azimuth           BAZ = 270  0  1.2914 From North
        ( DirectProblem x (DMS (90, 0, 0)) d
        , DirectSolution
            (DMS (45, 0, 0), DMS (-179, 59, 58.17367)) 
            (Just (DMS (270, 0, 1.2914)))
        )
    ,

--    Second Station :
--    ----------------
--     LAT =  44 59 58.70424 North
--     LON = 180  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 180  0  0.0000 From North
--   Back azimuth           BAZ =   0  0  0.0000 From North
        ( DirectProblem x (DMS (180, 0, 0)) d
        , DirectSolution
            (DMS (44, 59, 58.70424), DMS (180, 0, 0)) 
            (Just (DMS (0, 0, 0)))
        )
    ,

--    Second Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON = 179 59 58.17367 East
-- 
--   Forward azimuth        FAZ = 270  0  0.0000 From North
--   Back azimuth           BAZ =  89 59 58.7086 From North
        ( DirectProblem x (DMS (270, 0, 0)) d
        , DirectSolution
            (DMS (45, 0, 0), DMS (179, 59, 58.17367)) 
            (Just (DMS (89, 559, 58.7086)))
        )
    ]
    where
        x = (DMS (45, 0, 0), DMS (180, 0, 0)) 
        d = TaskDistance $ [u| 40 m |]
