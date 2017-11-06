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

module Flight.LatLng
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , DegToRad
    , RadToDeg
    , degToRadLL
    , radToDegLL
    , earthRadius
    ) where

import Data.Proxy
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Flight.Units ()
import qualified Flight.LatLng.Double as D
import qualified Flight.LatLng.Float as F
import qualified Flight.LatLng.Rational as R
--
-- | The radius of the earth in the FAI sphere is 6,371 km.
earthRadius :: Num a => Quantity a [u| m |]
earthRadius = [u| 6371000 m |]

newtype Lat a u = Lat (Quantity a u) deriving (Eq, Ord)
newtype Lng a u = Lng (Quantity a u) deriving (Eq, Ord)
newtype LatLng a u = LatLng (Lat a u, Lng a u) deriving (Eq, Ord)

-- SEE: https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html
type family F a :: Bool where
    F Double = 'True
    F Float = 'True
    F Rational = 'True
    F a = 'False

instance (F a ~ flag, ShowLat flag a u, KnownUnit (Unpack u)) => Show (Lat a u) where
  show = showLat (Proxy :: Proxy flag)

instance (F a ~ flag, ShowLng flag a u, KnownUnit (Unpack u)) => Show (Lng a u) where
  show = showLng (Proxy :: Proxy flag)

class (KnownUnit (Unpack u)) => ShowLat (flag :: Bool) a u where
    showLat :: Proxy flag -> Lat a u -> String

class (KnownUnit (Unpack u)) => ShowLng (flag :: Bool) a u where
    showLng :: Proxy flag -> Lng a u -> String

instance (KnownUnit (Unpack u), Show a) => ShowLat 'False a u where
    showLat _ (Lat lat) = show lat

instance (KnownUnit (Unpack u)) => ShowLat 'True Double u where
    showLat _ (Lat lat) = D.showAngle lat

instance (KnownUnit (Unpack u)) => ShowLat 'True Float u where
    showLat _ (Lat lat) = F.showAngle lat

instance (KnownUnit (Unpack u)) => ShowLat 'True Rational u where
    showLat _ (Lat lat) = R.showAngle lat

instance (KnownUnit (Unpack u), Show a) => ShowLng 'False a u where
    showLng _ (Lng lng) = show lng

instance (KnownUnit (Unpack u)) => ShowLng 'True Double u where
    showLng _ (Lng lng) = D.showAngle lng

instance (KnownUnit (Unpack u)) => ShowLng 'True Float u where
    showLng _ (Lng lng) = F.showAngle lng

instance (KnownUnit (Unpack u)) => ShowLng 'True Rational u where
    showLng _ (Lng lng) = R.showAngle lng

instance
    (KnownUnit (Unpack u), Show (Lat a u), Show (Lng a u))
    => Show (LatLng a u) where
    show (LatLng (lat, lng)) = "(" ++ show lat ++ ", " ++ show lng ++ ")"

type DegToRad a = Quantity a [u| deg |] -> Quantity a [u| rad |]
type RadToDeg a = Quantity a [u| rad |] -> Quantity a [u| deg |]

-- | Conversion of a lat/lng pair from degrees to radians.
degToRadLL :: DegToRad a -> LatLng a [u| deg |] -> LatLng a [u| rad |]
degToRadLL degToRad (LatLng (Lat lat, Lng lng)) =
    LatLng (Lat lat', Lng lng')
    where
        lat' = degToRad lat
        lng' = degToRad lng

-- | Conversion of a lat/lng pair from radians to degrees.
radToDegLL :: RadToDeg a -> LatLng a [u| rad |] -> LatLng a [u| deg |]
radToDegLL radToDeg (LatLng (Lat lat, Lng lng)) =
    LatLng (Lat lat', Lng lng')
    where
        lat' = radToDeg lat
        lng' = radToDeg lng
