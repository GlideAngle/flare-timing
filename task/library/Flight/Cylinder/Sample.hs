{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Flight.Cylinder.Sample
    ( TrueCourse(..)
    , Samples(..)
    , Tolerance(..)
    , ZonePoint(..)
    , SampleParams(..)
    , CircumSample
    , fromRationalZonePoint
    ) where

import Prelude hiding (span)
import Data.Ratio ((%), numerator, denominator)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..))
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , Bearing(..)
    , fromRationalRadius
    , fromRationalZone
    )
import Flight.Units (showRadian)

type CircumSample a =
    (Real a, Fractional a)
    => SampleParams a
    -> Bearing
    -> Maybe (ZonePoint a)
    -> Zone a
    -> ([ZonePoint a], [TrueCourse])

newtype TrueCourse =
    TrueCourse (Quantity Rational [u| rad |])
    deriving (Eq, Ord)

instance Show TrueCourse where
    show (TrueCourse tc) = "tc = " ++ showRadian tc

instance Num TrueCourse where
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

instance Fractional TrueCourse where
    fromRational tc = TrueCourse (MkQuantity tc)

    recip (TrueCourse (MkQuantity x)) =
        TrueCourse (MkQuantity (denominator x % numerator x))

newtype Samples = Samples { unSamples :: Integer } deriving (Eq, Ord, Show)
newtype Tolerance a = Tolerance { unTolerance :: a } deriving (Eq, Ord, Show)

data ZonePoint a
    = ZonePoint
        { sourceZone :: Zone a
        -- ^ This is the zone that generated the point.
        , point :: LatLng [u| rad |]
        -- ^ A point on the edge of this zone.
        , radial :: Bearing
        -- ^ A point on the edge of this zone with this bearing from
        -- the origin.
        , orbit :: Radius a
        -- ^ A point on the edge of this zone at this distance from the
        -- origin.
        }

fromRationalZonePoint :: Fractional a => ZonePoint Rational -> ZonePoint a
fromRationalZonePoint ZonePoint{..} =
    ZonePoint
        { sourceZone = fromRationalZone sourceZone
        , point = point
        , radial = radial
        , orbit = fromRationalRadius orbit
        }

data SampleParams a
    = SampleParams
        { spSamples :: Samples
        , spTolerance :: Tolerance a
        }
