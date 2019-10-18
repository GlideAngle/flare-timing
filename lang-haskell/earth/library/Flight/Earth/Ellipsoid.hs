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
    , GeodeticDirect(..)
    , GeodeticInverse(..)
    , GeodeticAccuracy(..)
    , Andoyer(..)
    , defaultGeodeticAccuracy
    , wgs84
    , nad83
    , bessel
    , hayford
    , clarke
    , bedfordClarke
    , flattening
    , polarRadius
    , toRationalEllipsoid
    , tooFar
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Zone (Radius(..), QRadius)

data Andoyer
    = AndoyerLambert
    | ForsytheAndoyerLambert
    | FsAndoyer

data Ellipsoid a =
    Ellipsoid
        { equatorialR :: QRadius a [u| m |]
        -- ^ Equatorial radius or semi-major axis of the ellipsoid.
        , recipF :: a
        -- ^ Reciprocal of the flattening of the ellipsoid or 1/ƒ.
        }
    deriving (Eq, Ord, Show, Generic)

-- NOTE: QRadius ToJSON and FromJSON instances only where a = Double
deriving instance ToJSON (Ellipsoid Double)
deriving instance FromJSON (Ellipsoid Double)

data AbnormalLatLng
    = LatUnder
    | LatOver
    | LngUnder
    | LngOver

data GeodeticDirect a
    = GeodeticDirectAbnormal AbnormalLatLng
    -- ^ Vincenty requires normalized latitude and longitude inputs, in radians
    -- the equivalent of -90 <= latitude <= 90 and -180 <= longitude <= 180
    -- degrees.
    | GeodeticDirectEquatorial
    -- ^ Vincenty's solution to the direct problem is indeterminate if the
    -- points are equatorial, checked for when abs λ > π.
    | GeodeticDirectAntipodal
    -- ^ Vincenty's solution to the direct problem is indeterminate if the
    -- points are antipodal, checked for when abs λ > π.
    | GeodeticDirect a
    -- ^ Vincenty's solution to the inverse problem.

data GeodeticInverse a
    = GeodeticInverseAbnormal AbnormalLatLng
    -- ^ Vincenty requires normalized latitude and longitude inputs, in radians
    -- the equivalent of -90 <= latitude <= 90 and -180 <= longitude <= 180
    -- degrees.
    | GeodeticInverseAntipodal
    -- ^ Vincenty's solution to the inverse problem is indeterminate if the
    -- points are antipodal, checked for when abs λ > π.
    | GeodeticInverse a
    -- ^ Vincenty's solution to the inverse problem.

toRationalEllipsoid :: Real a => Ellipsoid a -> Ellipsoid Rational
toRationalEllipsoid Ellipsoid{equatorialR = Radius r, recipF} =
    Ellipsoid
        { equatorialR = Radius $ toRational' r
        , recipF = toRational recipF
        }

-- SEE: https://en.wikipedia.org/wiki/World_Geodetic_System
-- https://en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS_84
wgs84 :: Fractional a => Ellipsoid a
wgs84 =
    Ellipsoid
        { equatorialR = Radius [u| 6378137 m |]
        , recipF = 298.257223563
        }

-- | As used by the National Geodetic Survey tool inverse when selecting the
-- ellipsoid 1) GRS80 / WGS84  (NAD83)
-- SEE: https://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/
nad83 :: Fractional a => Ellipsoid a
nad83 =
    wgs84{recipF = 298.25722210088}

-- | The Bessel ellipsoid from Vincenty 1975. Note that the flattening from
-- Wikipedia for the Bessel ellipsoid is 299.1528153513233 not 299.1528128.
-- SEE: https://en.wikipedia.org/wiki/Bessel_ellipsoid
bessel :: Fractional a => Ellipsoid a
bessel =
    Ellipsoid
        { equatorialR = Radius [u| 6377397.155 m |]
        , recipF = 299.1528128
        }

-- | The International ellipsoid 1924 also known as the Hayford ellipsoid from
-- Vincenty 1975.
-- SEE: https://en.wikipedia.org/wiki/Hayford_ellipsoid
hayford :: Fractional a => Ellipsoid a
hayford =
    Ellipsoid
        { equatorialR = Radius [u| 6378388 m |]
        , recipF = 297
        }

-- | Clarke's 1866 ellipsoid approximated in metres.
-- "Clarke actually defined his 1866 spheroid as
-- a = 20,926,062 British feet,
-- b = 20,855,121 British feet"
-- SEE: https://en.wikipedia.org/wiki/North_American_Datum
clarke :: Fractional a => Ellipsoid a
clarke =
    Ellipsoid
        { equatorialR = Radius [u| 6378206.4 m |]
        , recipF = 294.978698214
        }

-- | The ellipsoid used in Evaluation Direct and Inverse Geodetic Algorithms,
-- by Paul Delorme, Bedford Institute of Oceanography, Dartmouth, Nova Scotia,
-- Canada, 11978.
bedfordClarke :: Fractional a => Ellipsoid a
bedfordClarke = clarke{recipF = 294.9786986}

-- | The flattening of the ellipsoid.
flattening :: Fractional a => Ellipsoid a -> a
flattening Ellipsoid{recipF} =
    recip recipF

-- | The polar radius or semi-minor axis of the ellipsoid.
polarRadius :: Fractional a => Ellipsoid a -> Quantity a [u| m |]
polarRadius e@Ellipsoid{equatorialR = Radius (MkQuantity r)} =
    MkQuantity $ r * (1 - flattening e)

newtype GeodeticAccuracy a = GeodeticAccuracy a

defaultGeodeticAccuracy :: GeodeticAccuracy Rational
defaultGeodeticAccuracy = GeodeticAccuracy $ 1 % 1000000000000

tooFar :: Num a => QTaskDistance a [u| m |]
tooFar = TaskDistance [u| 20000000 m |]
