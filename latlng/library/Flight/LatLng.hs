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
    ) where

import System.Random
import Data.Proxy
import Data.UnitsOfMeasure (KnownUnit, Unpack, u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)
import Flight.Units ()
import qualified Flight.LatLng.Double as D
import qualified Flight.LatLng.Float as F
import qualified Flight.LatLng.Rational as R
--
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

instance (Fractional a, Convertible u [u| deg |]) => Bounded (Lat a u) where
    minBound = Lat $ convert [u| - 90 deg |]
    maxBound = Lat $ convert [u| 90 deg |]

instance (Fractional a, Convertible u [u| deg |]) => Bounded (Lng a u) where
    minBound = Lng $ convert [u| - 180 deg |]
    maxBound = Lng $ convert [u| 180 deg |]

instance
    (Real a, Fractional a, Convertible u [u| deg |])
    => Random (Lat a u) where
    randomR (Lat (MkQuantity lo), Lat (MkQuantity hi)) g =
        (Lat z, g')
        where
            (n, g') = next g
            (a, b) = genRange g

            dn :: Double
            dn = realToFrac hi - realToFrac lo

            dd :: Double
            dd = fromIntegral (b - a + 1)

            scale :: Double
            scale = dn / dd

            y :: Quantity Double u
            y = MkQuantity $ scale * fromIntegral (n - a) + realToFrac lo

            z :: Quantity a u
            z = fromRational' . toRational' . convert $ y

    random = randomR (Lat $ convert [u| - 90 deg |], Lat $ convert [u| 90 deg |])

instance
    (Real a, Fractional a, Convertible u [u| deg |])
    => Random (Lng a u) where
    randomR (Lng (MkQuantity lo), Lng (MkQuantity hi)) g =
        (Lng z, g')
        where
            (n, g') = next g
            (a, b) = genRange g

            dn :: Double
            dn = realToFrac hi - realToFrac lo

            dd :: Double
            dd = fromIntegral (b - a + 1)

            scale :: Double
            scale = dn / dd

            y :: Quantity Double u
            y = MkQuantity $ scale * fromIntegral (n - a) + realToFrac lo

            z :: Quantity a u
            z = fromRational' . toRational' . convert $ y

    random = randomR (Lng $ convert [u| - 180 deg |], Lng $ convert [u| 180 deg |])
