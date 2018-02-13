{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Ellipsoid
    ( Ellipsoid(..)
    , AbnormalLatLng(..)
    , VincentyInverse(..)
    , VincentyAccuracy(..)
    , defaultVincentyAccuracy
    , wgs84
    , flattening
    , toRationalEllipsoid
    ) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()

data Ellipsoid a =
    Ellipsoid
        { semiMajor :: Quantity a [u| m |]
        , semiMinor :: Quantity a [u| m |]
        }

data AbnormalLatLng
    = LatUnder
    | LatOver
    | LngUnder
    | LngOver

data VincentyInverse a
    = VincentyAbnormal AbnormalLatLng
    -- ^ Vincenty requires normalized latitude and longitude inputs, in radians
    -- the equivalent of -90 <= latitude <= 90 and -180 <= longitude <= 180
    -- degrees.
    | VincentyAntipodal
    -- ^ Vincenty's solution to the inverse problem is indeterminate if the
    -- points are antipodal, checked for when abs λ > π.
    | VincentyInverse a
    -- ^ Vincenty's solution to the inverse problem.

toRationalEllipsoid :: Real a => Ellipsoid a -> Ellipsoid Rational
toRationalEllipsoid Ellipsoid{semiMajor, semiMinor} =
    Ellipsoid
        { semiMajor = toRational' semiMajor
        , semiMinor = toRational' semiMinor
        }

-- SEE: https://en.wikipedia.org/wiki/World_Geodetic_System
wgs84 :: Fractional a => Ellipsoid a
wgs84 =
    Ellipsoid
        { semiMajor = [u| 6378137 m |]
        , semiMinor = [u| 6356752.3142 m |]
        }

flattening :: Fractional a => Ellipsoid a -> a
flattening Ellipsoid{semiMajor, semiMinor} =
    1 - b / a
    where
        MkQuantity a = semiMajor
        MkQuantity b = semiMinor

newtype VincentyAccuracy a = VincentyAccuracy a

defaultVincentyAccuracy :: VincentyAccuracy Rational
defaultVincentyAccuracy = VincentyAccuracy $ 1 % 1000000000000
