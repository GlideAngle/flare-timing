{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

-- SEE: https://en.wikipedia.org/wiki/Vincenty%27s_formulae#Inverse_problem
-- Notation used in the Vincenty formulae
--
-- a
-- length of semi-major axis of the ellipsoid (radius at equator) (6378137.0
-- metres in WGS-84)
--
-- ƒ
-- flattening of the ellipsoid
-- (1/298.257223563 in WGS-84)
--
-- b = (1 − ƒ) a
-- length of semi-minor axis of the ellipsoid (radius at the poles)
-- (6356752.314245 meters in WGS-84)
--
-- Φ1, Φ2
-- latitude of the points
--
-- U1 = arctan[(1 − ƒ) tan Φ1]
-- U2 = arctan[(1 − ƒ) tan Φ2]
-- reduced latitude (latitude on the auxiliary sphere)
--
-- L = L2 − L1
-- difference in longitude of two points
--
-- λ1, λ2
-- longitude of the points on the auxiliary sphere
--
-- α1, α2
-- forward azimuths at the points
--
-- α
-- azimuth at the equator
--
-- s
-- ellipsoidal distance between the two points
--
-- σ
-- arc length between points on the auxiliary sphere
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
