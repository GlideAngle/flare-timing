module DegMinSec (DMS(..), toDeg) where

import Prelude hiding (min)

newtype DMS = DMS (Int, Int, Double)

instance Show DMS where
    show = showDMS

showDMS :: DMS -> String
showDMS (DMS (deg, 0, 0)) =
    show deg ++ "°"
showDMS (DMS (deg, min, 0)) =
    show deg ++ "°" ++ show min ++ "'"
showDMS (DMS (deg, min, sec)) =
    show deg ++ "°" ++ show min ++ "'" ++ show sec ++ "''"

toDeg :: DMS -> Double
toDeg (DMS (deg, min, s)) =
    signum d * (abs d + m / 60 + s / 3600)
        where
            d = fromIntegral deg
            m = fromIntegral min 
