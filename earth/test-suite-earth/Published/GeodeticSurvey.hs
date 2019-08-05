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

import Flight.Units ()
import Flight.Geodesy
    ( GeodesyProblems(..)
    , DProb, DSoln
    , IProb, ISoln
    )
import qualified Published.GeodeticSurvey.LatN90LngW180S40 as LatN90LngW180S40
import qualified Published.GeodeticSurvey.LatN45LngW180S40 as LatN45LngW180S40
import qualified Published.GeodeticSurvey.LatN00LngW180S40 as LatN00LngW180S40
import qualified Published.GeodeticSurvey.LatN00LngE000S40 as LatN00LngE000S40
import qualified Published.GeodeticSurvey.LatS90LngE180S40 as LatS90LngE180S40
import qualified Published.GeodeticSurvey.Meridian as Meridian
import qualified Published.GeodeticSurvey.Parallel as Parallel

inverseProblems :: [IProb]
inverseProblems =
    fst <$> (inversePairs fwds ++ revs)

inverseSolutions :: [ISoln]
inverseSolutions =
    snd <$> (inversePairs fwds ++ revs)

directProblems :: [DProb]
directProblems =
    fst <$> (directPairs revs ++ fwds)

directSolutions :: [DSoln]
directSolutions =
    snd <$> (directPairs revs ++ fwds)

inversePairs :: [(DProb, DSoln)] -> [(IProb, ISoln)]
inversePairs xs =
    catMaybes $ uncurry inverse <$> xs

directPairs :: [(IProb, ISoln)] -> [(DProb, DSoln)]
directPairs xs =
    catMaybes $ uncurry direct <$> xs

fwds :: [(DProb, DSoln)]
fwds =
    concat
    [ LatN90LngW180S40.fwd
    , LatN45LngW180S40.fwd
    , LatN00LngW180S40.fwd
    , LatN00LngE000S40.fwd
    , LatS90LngE180S40.fwd
    ]

revs :: [(IProb, ISoln)]
revs =
    Meridian.rev ++ Parallel.rev
