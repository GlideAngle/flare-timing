{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Geo
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
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
import Flight.Units ()

newtype Lat u = Lat (Quantity Rational u) deriving (Eq, Ord)
newtype Lng u = Lng (Quantity Rational u) deriving (Eq, Ord)
newtype LatLng u = LatLng (Lat u, Lng u) deriving (Eq, Ord)

deriving instance (KnownUnit (Unpack u)) => Show (Lat u)
deriving instance (KnownUnit (Unpack u)) => Show (Lng u)

deriving instance (KnownUnit (Unpack u)) => Show (LatLng u)

newtype Epsilon = Epsilon Rational deriving (Eq, Ord, Show)

-- | Convert any 'Real' quantity into a 'Rational' type ('toRational').
toRational' :: Real a => Quantity a u -> Quantity Rational u
toRational' (MkQuantity x) = MkQuantity (toRational x)

-- | Conversion of degrees to radians.
degToRad :: Epsilon
         -> Quantity Rational [u| deg |]
         -> Quantity Rational [u| rad |]
degToRad (Epsilon eps) (MkQuantity x) =
    MkQuantity $ x * F.pi eps / (180 % 1)

-- | Conversion of a lat/lng pair from degrees to radians.
degToRadLL :: Epsilon -> LatLng [u| deg |] -> LatLng [u| rad |]
degToRadLL e (LatLng (Lat lat, Lng lng)) =
    LatLng (Lat lat', Lng lng')
    where
        lat' = degToRad e lat
        lng' = degToRad e lng

-- | Conversion of radians to degrees.
radToDeg :: Epsilon
         -> Quantity Rational [u| rad |]
         -> Quantity Rational [u| deg |]
radToDeg (Epsilon eps) (MkQuantity x) =
    MkQuantity $ x * (180 % 1) / F.pi eps

-- | Conversion of a lat/lng pair from radians to degrees.
radToDegLL :: Epsilon -> LatLng [u| rad |] -> LatLng [u| deg |]
radToDegLL e (LatLng (Lat lat, Lng lng)) =
    LatLng (Lat lat', Lng lng')
    where
        lat' = radToDeg e lat
        lng' = radToDeg e lng

defEps :: Epsilon
defEps = Epsilon $ 1 % 1000000000

-- | The radius of the earth in the FAI sphere is 6,371 km.
earthRadius :: Quantity Rational [u| m |]
earthRadius = [u| 6371000 % 1 m |]
