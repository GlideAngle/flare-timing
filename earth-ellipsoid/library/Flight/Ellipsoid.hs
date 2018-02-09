{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Ellipsoid
    ( Ellipsoid(..)
    , wgs84
    , flattening
    , toRationalEllipsoid
    ) where

import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()

data Ellipsoid a =
    Ellipsoid
        { semiMajor :: Quantity a [u| m |]
        , semiMinor :: Quantity a [u| m |]
        }

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
