module Published.GeodeticSurvey.Parallel (rev) where

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
rev :: [(IProb, ISoln)]
rev =
    [
--    First  Station :
--    ----------------
--     LAT =   0  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =   0  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270  0  0.0000 From North
--   Back azimuth           BAZ =  90  0  0.0000 From North
--   Ellipsoidal distance     S =    111319.4908 m
        ( InverseProblem (DMS (0, 0, 0), d0) (DMS (0, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 111319.4908 m |])
            (DMS (270, 0, 0))
            (Just $ DMS (90, 0, 0))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =   5  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =   5  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270  2 36.8843 From North
--   Back azimuth           BAZ =  89 57 23.1157 From North
--   Ellipsoidal distance     S =    110898.6955 m
        ( InverseProblem (DMS (5, 0, 0), d0) (DMS (5, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 110898.6955 m |])
            (DMS (270, 2, 36.8843))
            (Just $ DMS (89, 57, 23.1157))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  10  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  10  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270  5 12.5745 From North
--   Back azimuth           BAZ =  89 54 47.4255 From North
--   Ellipsoidal distance     S =    109639.3221 m
        ( InverseProblem (DMS (10, 0, 0), d0) (DMS (10, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 109639.3221 m |])
            (DMS (270, 5, 12.5745))
            (Just $ DMS (89, 54, 47.4255))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  15  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  15  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270  7 45.8854 From North
--   Back azimuth           BAZ =  89 52 14.1146 From North
--   Ellipsoidal distance     S =    107550.3973 m
        ( InverseProblem (DMS (15, 0, 0), d0) (DMS (15, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 107550.3973 m |])
            (DMS (270, 7, 45.8854))
            (Just $ DMS (89, 52, 14.1146))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  20  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  20  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 10 15.6501 From North
--   Back azimuth           BAZ =  89 49 44.3499 From North
--   Ellipsoidal distance     S =    104646.9309 m
        ( InverseProblem (DMS (20, 0, 0), d0) (DMS (20, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 104646.9309 m |])
            (DMS (270, 10, 15.6501))
            (Just $ DMS (89, 49, 44.3499))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  25  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  25  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 12 40.7288 From North
--   Back azimuth           BAZ =  89 47 19.2712 From North
--   Ellipsoidal distance     S =    100949.8614 m
        ( InverseProblem (DMS (25, 0, 0), d0) (DMS (25, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 100949.8614 m |])
            (DMS (270, 12, 40.7288))
            (Just $ DMS (89, 47, 19.2712))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  30  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  30  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 15  0.0172 From North
--   Back azimuth           BAZ =  89 44 59.9828 From North
--   Ellipsoidal distance     S =     96485.9741 m
        ( InverseProblem (DMS (30, 0, 0), d0) (DMS (30, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 96485.9741 m |])
            (DMS (270, 15, 0.0172))
            (Just $ DMS (89, 44, 59.9828))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  35  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  35  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 17 12.4553 From North
--   Back azimuth           BAZ =  89 42 47.5447 From North
--   Ellipsoidal distance     S =     91287.7885 m
        ( InverseProblem (DMS (35, 0, 0), d0) (DMS (35, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 91287.7885 m |])
            (DMS (270, 17, 12.4553))
            (Just $ DMS (89, 42, 47.5447))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  40  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  40  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 19 17.0350 From North
--   Back azimuth           BAZ =  89 40 42.9650 From North
--   Ellipsoidal distance     S =     85393.4091 m
        ( InverseProblem (DMS (40, 0, 0), d0) (DMS (40, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 85393.4091 m |])
            (DMS (270, 19, 17.0350))
            (Just $ DMS (89, 40, 42.9650))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  45  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 21 12.8084 From North
--   Back azimuth           BAZ =  89 38 47.1916 From North
--   Ellipsoidal distance     S =     78846.3347 m
        ( InverseProblem (DMS (45, 0, 0), d0) (DMS (45, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 78846.3347 m |])
            (DMS (270, 21, 12.8084))
            (Just $ DMS (89, 38, 47.1916))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  50  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  50  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 22 58.8945 From North
--   Back azimuth           BAZ =  89 37  1.1055 From North
--   Ellipsoidal distance     S =     71695.2196 m
        ( InverseProblem (DMS (50, 0, 0), d0) (DMS (50, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 71695.2196 m |])
            (DMS (270, 22, 58.8945))
            (Just $ DMS (89, 37, 1.1055))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  55  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  55  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 24 34.4860 From North
--   Back azimuth           BAZ =  89 35 25.5140 From North
--   Ellipsoidal distance     S =     63993.5843 m
        ( InverseProblem (DMS (55, 0, 0), d0) (DMS (55, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 63993.5843 m |])
            (DMS (270, 24, 34.4860))
            (Just $ DMS (89, 35, 25.5140))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  60  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  60  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 25 58.8556 From North
--   Back azimuth           BAZ =  89 34  1.1444 From North
--   Ellipsoidal distance     S =     55799.4704 m
        ( InverseProblem (DMS (60, 0, 0), d0) (DMS (60, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 55799.4704 m |])
            (DMS (270, 25, 58.8556))
            (Just $ DMS (89, 34, 1.1444))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  65  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  65  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 27 11.3614 From North
--   Back azimuth           BAZ =  89 32 48.6386 From North
--   Ellipsoidal distance     S =     47175.0392 m
        ( InverseProblem (DMS (65, 0, 0), d0) (DMS (65, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 47175.0392 m |])
            (DMS (270, 27, 11.3614))
            (Just $ DMS (89, 32, 48.6386))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  70  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  70  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 28 11.4517 From North
--   Back azimuth           BAZ =  89 31 48.5483 From North
--   Ellipsoidal distance     S =     38186.1133 m
        ( InverseProblem (DMS (70, 0, 0), d0) (DMS (70, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 38186.1133 m |])
            (DMS (270, 28, 11.4517))
            (Just $ DMS (89, 31, 48.5483))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  75  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  75  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 28 58.6694 From North
--   Back azimuth           BAZ =  89 31  1.3306 From North
--   Ellipsoidal distance     S =     28901.6635 m
        ( InverseProblem (DMS (75, 0, 0), d0) (DMS (75, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 28901.6635 m |])
            (DMS (270, 28, 58.6694))
            (Just $ DMS (89, 31, 1.3306))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  80  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  80  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 29 32.6553 From North
--   Back azimuth           BAZ =  89 30 27.3447 From North
--   Ellipsoidal distance     S =     19393.2468 m
        ( InverseProblem (DMS (80, 0, 0), d0) (DMS (80, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 19393.2468 m |])
            (DMS (270, 29, 32.6553))
            (Just $ DMS (89, 30, 27.3447))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  85  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  85  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ = 270 29 53.1508 From North
--   Back azimuth           BAZ =  89 30  6.8492 From North
--   Ellipsoidal distance     S =      9734.4000 m
        ( InverseProblem (DMS (85, 0, 0), d0) (DMS (85, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 9734.4000 m |])
            (DMS (270, 29, 53.1508))
            (Just $ DMS (89, 30, 6.8492))
        )
    ,

--    First  Station :
--    ----------------
--     LAT =  90  0  0.00000 North
--     LON =   0  0  0.00000 East
-- 
--    Second Station :
--    ----------------
--     LAT =  90  0  0.00000 North
--     LON =   1  0  0.00000 West
-- 
--   Forward azimuth        FAZ =   0  0  0.0000 From North
--   Back azimuth           BAZ =   0  0  0.0000 From North
--   Ellipsoidal distance     S =         0.0000 m
        ( InverseProblem (DMS (90, 0, 0), d0) (DMS (90, 0, 0), d1)
        , InverseSolution
            (TaskDistance [u| 0.0000 m |])
            d0
            (Just d0)
        )
    ]
    where
        d0 = DMS (0, 0, 0)
        d1 = DMS (-1, 0, 0)
