module ToLatLng (ToLatLng, toLatLngD, toLatLngR) where

import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))

type ToLatLng a = (Double, Double) -> LatLng a [u| rad |]

-- | The input pair is in degrees while the output is in radians.
toLatLngD :: ToLatLng Double
toLatLngD (lat, lng) =
    LatLng (Lat lat'', Lng lng'')
        where
            lat' = (MkQuantity lat) :: Quantity Double [u| deg |]
            lng' = (MkQuantity lng) :: Quantity Double [u| deg |]
            lat'' = convert lat' :: Quantity Double [u| rad |]
            lng'' = convert lng' :: Quantity Double [u| rad |]

toLatLngR :: ToLatLng Rational
toLatLngR (lat, lng) =
    LatLng (Lat lat'', Lng lng'')
        where
            lat' = (MkQuantity $ toRational lat) :: Quantity Rational [u| deg |]
            lng' = (MkQuantity $ toRational lng) :: Quantity Rational [u| deg |]
            lat'' = convert lat' :: Quantity Rational [u| rad |]
            lng'' = convert lng' :: Quantity Rational [u| rad |]
