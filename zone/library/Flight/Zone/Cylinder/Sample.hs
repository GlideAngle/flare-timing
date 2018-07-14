module Flight.Zone.Cylinder.Sample
    ( TrueCourse(..)
    , Samples(..)
    , Tolerance(..)
    , ZonePoint(..)
    , SampleParams(..)
    , fromRationalZonePoint
    ) where

import Data.UnitsOfMeasure (u, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..), QLat, QLng)
import Flight.Zone
    ( Zone(..)
    , Bearing(..)
    , fromRationalRadius
    , fromRationalZone
    , fromRationalLatLng
    )
import Flight.Units (showRadian)
import Flight.Zone.Radius (QRadius)
import Flight.Zone.Bearing (QBearing)

newtype TrueCourse a =
    TrueCourse (Quantity a [u| rad |])
    deriving (Eq, Ord)

instance Real a => Show (TrueCourse a) where
    show (TrueCourse tc) = "tc = " ++ showRadian (toRational' tc)

instance Num a => Num (TrueCourse a) where
    (+) (TrueCourse (MkQuantity a)) (TrueCourse (MkQuantity b)) =
        TrueCourse (MkQuantity $ a + b)

    (*) (TrueCourse (MkQuantity a)) (TrueCourse (MkQuantity b)) =
        TrueCourse (MkQuantity $ a * b)

    negate (TrueCourse (MkQuantity tc)) =
        TrueCourse (MkQuantity $ negate tc)

    abs (TrueCourse (MkQuantity tc)) =
        TrueCourse (MkQuantity $ abs tc)

    signum (TrueCourse (MkQuantity tc)) =
        TrueCourse (MkQuantity $ signum tc)

    fromInteger x =
        TrueCourse (MkQuantity $ fromInteger x)

instance Fractional a => Fractional (TrueCourse a) where
    fromRational tc = TrueCourse (MkQuantity $ fromRational tc)

    recip (TrueCourse (MkQuantity x)) =
        TrueCourse (MkQuantity $ recip x)

newtype Samples = Samples { unSamples :: Integer } deriving (Eq, Ord, Show)
newtype Tolerance a = Tolerance { unTolerance :: a } deriving (Eq, Ord, Show)

data ZonePoint a
    = ZonePoint
        { sourceZone :: Zone a
        -- ^ This is the zone that generated the point.
        , point :: LatLng a [u| rad |]
        -- ^ A point on the edge of this zone.
        , radial :: QBearing a [u| rad |]
        -- ^ A point on the edge of this zone with this bearing from
        -- the origin.
        , orbit :: QRadius a [u| m |]
        -- ^ A point on the edge of this zone at this distance from the
        -- origin.
        }
    deriving Eq

deriving instance
    ( Show a
    , Real a
    , Fractional a
    , Show (QLat a [u| rad |])
    , Show (QLng a [u| rad |])
    )
    => Show (ZonePoint a)

fromRationalZonePoint
    :: (Eq a, Ord a, Fractional a)
    => ZonePoint Rational
    -> ZonePoint a
fromRationalZonePoint ZonePoint{..} =
    ZonePoint
        { sourceZone = fromRationalZone sourceZone
        , point = fromRationalLatLng point
        , radial = let Bearing b = radial in Bearing $ fromRational' b
        , orbit = fromRationalRadius orbit
        }

data SampleParams a
    = SampleParams
        { spSamples :: Samples
        , spTolerance :: Tolerance a
        }
