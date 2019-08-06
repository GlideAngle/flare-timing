module Published.GeodeticSurvey.Meridian (rev) where

import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))
import Flight.Geodesy (InverseProblem(..), InverseSolution(..), IProb, ISoln)

-- | With a common ellipsoid of;
--
--   Ellipsoid : GRS80 / WGS84  (NAD83)
--   Equatorial axis,    a   =    6378137.0000
--   Polar axis,         b   =    6356752.3141
--   Inverse flattening, 1/f =  298.25722210088
--
--   For each inverse solution the azimuths are;
--   Forward azimuth        FAZ =   0  0  0.0000 From North
--   Back azimuth           BAZ = 180  0  0.0000 From North
rev :: [(IProb, ISoln)]
rev =
    [

--    First  Station :
--    ----------------
--     LAT =   4 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =   5 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    110582.7384 m
        ( InverseProblem (DMS (4, 30, 0), d0) (DMS (5, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 110582.7384 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =   9 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  10 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    110607.7916 m
        ( InverseProblem (DMS (9, 30, 0), d0) (DMS (10, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 110607.7916 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  14 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  15 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    110648.7207 m
        ( InverseProblem (DMS (14, 30, 0), d0) (DMS (15, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 110648.7207 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  19 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  20 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    110704.3098 m
        ( InverseProblem (DMS (19, 30, 0), d0) (DMS (20, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 110704.3098 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  24 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  25 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    110772.9044 m
        ( InverseProblem (DMS (24, 30, 0), d0) (DMS (25, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 110772.9044 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  29 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  30 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    110852.4568 m
        ( InverseProblem (DMS (29, 30, 0), d0) (DMS (30, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 110852.4568 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  34 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  35 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    110940.5844 m
        ( InverseProblem (DMS (34, 30, 0), d0) (DMS (35, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 110940.5844 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  39 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  40 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111034.6377 m
        ( InverseProblem (DMS (39, 30, 0), d0) (DMS (40, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111034.6377 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  44 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  45 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111131.7777 m
        ( InverseProblem (DMS (44, 30, 0), d0) (DMS (45, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111131.7777 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  49 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  50 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111229.0593 m
        ( InverseProblem (DMS (49, 30, 0), d0) (DMS (50, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111229.0593 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  54 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  55 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111323.5206 m
        ( InverseProblem (DMS (54, 30, 0), d0) (DMS (55, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111323.5206 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  59 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  60 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111412.2734 m
        ( InverseProblem (DMS (59, 30, 0), d0) (DMS (60, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111412.2734 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  64 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  65 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111492.5926 m
        ( InverseProblem (DMS (64, 30, 0), d0) (DMS (65, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111492.5926 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  69 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  70 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111562.0033 m
        ( InverseProblem (DMS (69, 30, 0), d0) (DMS (70, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111562.0033 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  74 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  75 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111618.3593 m
        ( InverseProblem (DMS (74, 30, 0), d0) (DMS (75, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111618.3593 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  79 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  80 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111659.9135 m
        ( InverseProblem (DMS (79, 30, 0), d0) (DMS (80, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111659.9135 m |]) xz yz
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  84 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  85 30  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--   Ellipsoidal distance     S =    111685.3748 m
        ( InverseProblem (DMS (84, 30, 0), d0) (DMS (85, 30, 0), d0)
        , InverseSolution (TaskDistance [u| 111685.3748 m |]) xz yz
        )
    ]
    where
        d0 = DMS (0, 0, 0)
        xz = DMS (0, 0, 0)
        yz = Just $ DMS (180, 0, 0)
