module Greda (task1) where

import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.Zone (Zone(..), Radius(..))
import ToLatLng (ToLatLng)

gs1 :: ToLatLng a -> LatLng a [u| rad |]
gs1 toLL = toLL (43.82972999, 16.64243)

g35 :: ToLatLng a -> LatLng a [u| rad |]
g35 toLL = toLL (43.84411, 16.6599)

g44 :: ToLatLng a -> LatLng a [u| rad |]
g44 toLL = toLL (43.82292999, 16.61628999)

g10 :: ToLatLng a -> LatLng a [u| rad |]
g10 toLL = toLL (43.78045, 16.65744)

g36 :: ToLatLng a -> LatLng a [u| rad |]
g36 toLL = toLL (43.82341999, 16.58681999)

g17 :: ToLatLng a -> LatLng a [u| rad |]
g17 toLL = toLL (43.72863, 16.69474999)

g39 :: ToLatLng a -> LatLng a [u| rad |]
g39 toLL = toLL (43.75677, 16.62018)

task1 :: (Ord a, Num a) => ToLatLng a -> [Zone a]
task1 toLL =
    [ Cylinder (Radius [u|  400 m |]) $ gs1 toLL
    , Cylinder (Radius [u| 3000 m |]) $ g35 toLL
    , Cylinder (Radius [u|  400 m |]) $ g44 toLL
    , Cylinder (Radius [u|  400 m |]) $ g10 toLL
    , Cylinder (Radius [u| 1000 m |]) $ g36 toLL
    , Cylinder (Radius [u| 1000 m |]) $ g17 toLL
    , Cylinder (Radius [u| 1000 m |]) $ g39 toLL
    , Cylinder (Radius [u|  400 m |]) $ g39 toLL
    ]
