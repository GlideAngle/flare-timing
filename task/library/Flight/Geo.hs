{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}

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
    ) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Show (showUnit, showQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Flight.Units ()
import Data.Number.RoundingFunctions (dpRound)

newtype Lat u = Lat (Quantity Rational u) deriving (Eq, Ord)
newtype Lng u = Lng (Quantity Rational u) deriving (Eq, Ord)
newtype LatLng u = LatLng (Lat u, Lng u) deriving (Eq, Ord)

showAngle :: KnownUnit (Unpack u) => Quantity Rational u -> String
showAngle q@(MkQuantity x) =
    case showUnit q of
        "deg" -> show (fromRational (dpRound 3 x) :: Double) ++ "°"
        "rad" -> show (fromRational (dpRound 3 x) :: Double) ++ " rad"
        _ -> showQuantity q

instance (KnownUnit (Unpack u)) => Show (Lat u) where
    show (Lat lat) = showAngle lat

instance (KnownUnit (Unpack u)) => Show (Lng u) where
    show (Lng lng) = showAngle lng

instance (KnownUnit (Unpack u)) => Show (LatLng u) where
    show (LatLng (lat, lng)) =
        "(" ++ show lat ++ ", " ++ show lng ++ ")"

newtype Epsilon = Epsilon Rational deriving (Eq, Ord, Show)

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
defEps = Epsilon $ 1 % 1000000000000

-- | The radius of the earth in the FAI sphere is 6,371 km.
earthRadius :: Quantity Rational [u| m |]
earthRadius = [u| 6371000 % 1 m |]
