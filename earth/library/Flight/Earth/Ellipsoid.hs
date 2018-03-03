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
module Flight.Earth.Ellipsoid
    ( Ellipsoid(..)
    , AbnormalLatLng(..)
    , VincentyDirect(..)
    , VincentyInverse(..)
    , VincentyAccuracy(..)
    , defaultVincentyAccuracy
    , wgs84
    , bessel
    , hayford
    , flattening
    , polarRadius
    , toRationalEllipsoid
    ) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()

data Ellipsoid a =
    Ellipsoid
        { equatorialR :: Quantity a [u| m |]
        -- ^ Equatorial radius or semi-major axis of the ellipsoid.
        , recipF :: a
        -- ^ Reciprocal of the flattening of the ellipsoid or 1/ƒ.
        }

data AbnormalLatLng
    = LatUnder
    | LatOver
    | LngUnder
    | LngOver

data VincentyDirect a
    = VincentyDirectAbnormal AbnormalLatLng
    -- ^ Vincenty requires normalized latitude and longitude inputs, in radians
    -- the equivalent of -90 <= latitude <= 90 and -180 <= longitude <= 180
    -- degrees.
    | VincentyDirectEquatorial
    -- ^ Vincenty's solution to the direct problem is indeterminate if the
    -- points are equatorial, checked for when abs λ > π.
    | VincentyDirectAntipodal
    -- ^ Vincenty's solution to the direct problem is indeterminate if the
    -- points are antipodal, checked for when abs λ > π.
    | VincentyDirect a
    -- ^ Vincenty's solution to the inverse problem.

data VincentyInverse a
    = VincentyInverseAbnormal AbnormalLatLng
    -- ^ Vincenty requires normalized latitude and longitude inputs, in radians
    -- the equivalent of -90 <= latitude <= 90 and -180 <= longitude <= 180
    -- degrees.
    | VincentyInverseAntipodal
    -- ^ Vincenty's solution to the inverse problem is indeterminate if the
    -- points are antipodal, checked for when abs λ > π.
    | VincentyInverse a
    -- ^ Vincenty's solution to the inverse problem.

toRationalEllipsoid :: Real a => Ellipsoid a -> Ellipsoid Rational
toRationalEllipsoid Ellipsoid{equatorialR, recipF} =
    Ellipsoid
        { equatorialR = toRational' equatorialR
        , recipF = toRational recipF
        }

-- SEE: https://en.wikipedia.org/wiki/World_Geodetic_System
-- https://en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS_84
wgs84 :: Fractional a => Ellipsoid a
wgs84 =
    Ellipsoid
        { equatorialR = [u| 6378137 m |]
        , recipF = 298.257223563
        }

-- | The Bessel ellipsoid from Vincenty 1975. Note that the flattening from
-- Wikipedia for the Bessel ellipsoid is 299.1528153513233 not 299.1528128.
-- SEE: https://en.wikipedia.org/wiki/Bessel_ellipsoid
bessel :: Fractional a => Ellipsoid a
bessel =
    Ellipsoid
        { equatorialR = [u| 6377397.155 m |]
        , recipF = 299.1528128

        }

-- | The International ellipsoid 1924 also known as the Hayford ellipsoid from
-- Vincenty 1975.
-- SEE: https://en.wikipedia.org/wiki/Hayford_ellipsoid
hayford :: Fractional a => Ellipsoid a
hayford =
    Ellipsoid
        { equatorialR = [u| 6378388 m |]
        , recipF = 297
        }

-- | The flattening of the ellipsoid.
flattening :: Fractional a => Ellipsoid a -> a
flattening Ellipsoid{recipF} =
    recip recipF 

-- | The polar radius or semi-minor axis of the ellipsoid.
polarRadius :: Fractional a => Ellipsoid a -> Quantity a [u| m |]
polarRadius e@Ellipsoid{equatorialR = MkQuantity r} =
    MkQuantity $ r * (1 - flattening e)

newtype VincentyAccuracy a = VincentyAccuracy a

defaultVincentyAccuracy :: VincentyAccuracy Rational
defaultVincentyAccuracy = VincentyAccuracy $ 1 % 1000000000000
