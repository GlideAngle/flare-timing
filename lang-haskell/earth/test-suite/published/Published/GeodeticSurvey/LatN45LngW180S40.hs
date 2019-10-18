module Published.GeodeticSurvey.LatN45LngW180S40 (fwd) where

import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import Flight.Geodesy (DirectProblem(..), DirectSolution(..), DProb, DSoln)

-- | With a common first station and ellipsoidal distance of;
--
--    First  Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON = 180  0  0.00000 West
--
--   Ellipsoidal distance     S =        40.0000 m
fwd :: [(DProb, DSoln)]
fwd =
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
            (DMS (45, 0, 1.29576), DMS (-180, 0, 0)) 
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
            (DMS (44, 59, 58.70424), DMS (-180, 0, 0)) 
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
        x = (DMS (45, 0, 0), DMS (-180, 0, 0)) 
        d = TaskDistance [u| 40 m |]
