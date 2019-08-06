module Published.GeodeticSurvey.LatN90LngW180S40 (fwd) where

import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import Flight.Geodesy (DirectProblem(..), DirectSolution(..), DProb, DSoln)

-- | With a common first station and ellipsoidal distance of;
--
--    First  Station :
--    ----------------
--     LAT =  90  0  0.00000 North
--     LON = 180  0  0.00000 West
--   Ellipsoidal distance     S =        40.0000 m
fwd :: [(DProb, DSoln)]
fwd =
    [

--    Second Station :
--    ----------------
--     LAT =  89 59 58.71076 North
--     LON =   0  0  0.00000 East
-- 
--   Forward azimuth        FAZ =   0  0  0.0000 From North
--   Back azimuth           BAZ =   0  0  0.0000 From North
        ( DirectProblem x (DMS (0, 0, 0)) d
        , DirectSolution
            (DMS (89, 59, s), DMS (0, 0, 0)) 
            y
        )
    ,

--    Second Station :
--    ----------------
--     LAT =  89 59 58.71076 North
--     LON =  90  0  0.00000 West
-- 
--   Forward azimuth        FAZ =  90  0  0.0000 From North
--   Back azimuth           BAZ =   0  0  0.0000 From North
        ( DirectProblem x (DMS (90, 0, 0)) d
        , DirectSolution
            (DMS (89, 59, s), DMS (-90, 0, 0)) 
            y
        )
    ,

--    Second Station :
--    ----------------
--     LAT =  89 59 58.71076 North
--     LON = 180  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 180  0  0.0000 From North
--   Back azimuth           BAZ =   0  0  0.0000 From North
        ( DirectProblem x (DMS (180, 0, 0)) d
        , DirectSolution
            (DMS (89, 59, s), DMS (-180, 0, 0)) 
            y
        )
    ,

--    Second Station :
--    ----------------
--     LAT =  89 59 58.71076 North
--     LON =  90  0  0.00000 East
-- 
--   Forward azimuth        FAZ = 270  0  0.0000 From North
--   Back azimuth           BAZ =   0  0  0.0000 From North
        ( DirectProblem x (DMS (270, 0, 0)) d
        , DirectSolution
            (DMS (89, 59, s), DMS (90, 0, 0)) 
            y
        )
    ]
    where
        x = (DMS (90, 0, 0), DMS (-180, 0, 0)) 
        d = TaskDistance [u| 40 m |]
        y = Just (DMS (0, 0, 0))
        s = 58.71076
