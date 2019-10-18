module Published.GeodeticSurvey.LatN00LngE000S40 (fwd) where

import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import Flight.Geodesy (DirectProblem(..), DirectSolution(..), DProb, DSoln)

-- | With a common first station and ellipsoidal distance of;
--
--    First  Station :
--    ----------------
--     LAT =   0  0  0.00000 North
--     LON =   0  0  0.00000 East
--   Ellipsoidal distance     S =        40.0000 m
fwd :: [(DProb, DSoln)]
fwd =
    [

--    Second Station :
--    ----------------
--     LAT =   0  0  1.30229 North
--     LON =   0  0  0.00000 East
-- 
--   Forward azimuth        FAZ =   0  0  0.0000 From North
--   Back azimuth           BAZ = 180  0  0.0000 From North
        ( DirectProblem x (DMS (0, 0, 0)) d
        , DirectSolution
            (DMS (0, 0, 1.30229), snd x) 
            (Just (DMS (180, 0, 0)))
        )
    ,

--    Second Station :
--    ----------------
--     LAT =   0  0  0.00000 North
--     LON =   0  0  1.29357 East
-- 
--   Forward azimuth        FAZ =  90  0  0.0000 From North
--   Back azimuth           BAZ = 270  0  0.0000 From North
        ( DirectProblem x (DMS (90, 0, 0)) d
        , DirectSolution
            (fst x, DMS (0, 0, 1.29357)) 
            (Just (DMS (270, 0, 0)))
        )
    ,

--    Second Station :
--    ----------------
--     LAT =   0  0  1.30229 South
--     LON =   0  0  0.00000 East
-- 
--   Forward azimuth        FAZ = 180  0  0.0000 From North
--   Back azimuth           BAZ =   0  0  0.0000 From North
        ( DirectProblem x (DMS (180, 0, 0)) d
        , DirectSolution
            (DMS (0, 0, -1.30229), snd x)
            (Just (DMS (0, 0, 0)))
        )
    ,

--    Second Station :
--    ----------------
--     LAT =   0  0  0.00000 South
--     LON =   0  0  1.29357 West
-- 
--   Forward azimuth        FAZ = 270  0  0.0000 From North
--   Back azimuth           BAZ =  90  0  0.0000 From North
        ( DirectProblem x (DMS (270, 0, 0)) d
        , DirectSolution
            (fst x, DMS (0, 0, -1.29357)) 
            (Just (DMS (90, 0, 0)))
        )
    ]
    where
        x = (DMS (0, 0, 0), DMS (0, 0, 0)) 
        d = TaskDistance [u| 40 m |]
