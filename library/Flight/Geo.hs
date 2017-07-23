{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Geo
    ( LatLng(..)
    , Epsilon(..)
    , defEps
    , degToRad
    , degToRadLL
    , radToDeg
    , radToDegLL
    , earthRadius
    , toRational'
    ) where

import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Defs ()

newtype LatLng = LatLng (Rational, Rational) deriving (Eq, Ord, Show)

newtype Epsilon = Epsilon Rational deriving (Eq, Ord, Show)

-- | Convert any 'Real' quantity into a 'Rational' type ('toRational').
toRational' :: Real a => Quantity a u -> Quantity Rational u
toRational' (MkQuantity x) = MkQuantity (toRational x)

-- | Conversion of degrees to radians.
degToRad :: Epsilon -> Rational -> Rational
degToRad (Epsilon eps) x = x * F.pi eps / (180 % 1)

-- | Conversion of a lat/lng pair from degrees to radians.
degToRadLL :: Epsilon -> LatLng -> LatLng
degToRadLL e (LatLng (lat, lng)) =
    LatLng (degToRad e lat, degToRad e lng)

-- | Conversion of radians to degrees.
radToDeg :: Epsilon -> Rational -> Rational
radToDeg (Epsilon eps) x = x * (180 % 1) / F.pi eps

-- | Conversion of a lat/lng pair from radians to degrees.
radToDegLL :: Epsilon -> LatLng -> LatLng
radToDegLL e (LatLng (lat, lng)) =
    LatLng (radToDeg e lat, radToDeg e lng)

defEps :: Epsilon
defEps = Epsilon $ 1 % 1000000000

-- | The radius of the earth in the FAI sphere is 6,371 km.
earthRadius :: Quantity Rational [u| m |]
earthRadius = [u| 6371000 % 1 m |]
