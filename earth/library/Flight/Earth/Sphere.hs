{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Flight.Earth.Sphere (earthRadius) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()

-- | The radius of the earth in the FAI sphere is 6,371 km.
earthRadius :: Num a => Quantity a [u| m |]
earthRadius = [u| 6371000 m |]
