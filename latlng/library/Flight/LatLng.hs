{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.LatLng
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , DegToRad
    , RadToDeg
    , AzimuthFwd
    , AzimuthRev
    , degToRadLL
    , radToDegLL
    , fromDMS
    ) where

import System.Random
import Data.Proxy
import Data.UnitsOfMeasure (KnownUnit, Unpack, u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)
import Control.Monad (guard)
import Test.SmallCheck.Series as SC (Serial(..), (>>-), cons2, decDepth)
import Test.Tasty.QuickCheck as QC (Arbitrary(..), arbitraryBoundedRandom)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..), toDeg)
import qualified Flight.LatLng.Double as D
import qualified Flight.LatLng.Float as F
import qualified Flight.LatLng.Rational as R

-- | A function calculating the forward azimuth between two points given as
-- latitude longitude pairs in radians.
type AzimuthFwd a
    = LatLng a [u| rad |]
    -> LatLng a [u| rad |]
    -> Maybe (Quantity a [u| rad |])

-- | A function calculating the reverse azimuth between two points given as
-- latitude longitude pairs in radians.
type AzimuthRev a
    = LatLng a [u| rad |]
    -> LatLng a [u| rad |]
    -> Maybe (Quantity a [u| rad |])

newtype Lat a u = Lat (Quantity a u) deriving (Eq, Ord)
newtype Lng a u = Lng (Quantity a u) deriving (Eq, Ord)
newtype LatLng a u = LatLng (Lat a u, Lng a u) deriving (Eq, Ord)

fromDMS :: (DMS, DMS) -> LatLng Double [u| rad |]
fromDMS (lat, lng) =
    LatLng (Lat lat'', Lng lng'')
        where
            lat' :: Quantity Double [u| deg |]
            lat' = MkQuantity . toDeg $ lat

            lng' :: Quantity Double [u| deg |]
            lng' = MkQuantity . toDeg $ lng

            lat'' = convert lat' :: Quantity _ [u| rad |]
            lng'' = convert lng' :: Quantity _ [u| rad |]

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

instance
    ( KnownUnit (Unpack u)
    , Convertible u [u| deg |]
    )
    => ShowLat 'True Double u where
    showLat _ (Lat lat) = D.showAngle lat

instance (KnownUnit (Unpack u)) => ShowLat 'True Float u where
    showLat _ (Lat lat) = F.showAngle lat

instance (KnownUnit (Unpack u)) => ShowLat 'True Rational u where
    showLat _ (Lat lat) = R.showAngle lat

instance (KnownUnit (Unpack u), Show a) => ShowLng 'False a u where
    showLng _ (Lng lng) = show lng

instance
    ( KnownUnit (Unpack u)
    , Convertible u [u| deg |]
    )
    => ShowLng 'True Double u where
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
    ( Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Random (Lat a u) where
    randomR (Lat lo, Lat hi) g =
        (Lat . convert $ z, g')
        where
            (z, g') = qRandomR g lo hi

    random = randomR (Lat $ convert [u| - 90 deg |], Lat $ convert [u| 90 deg |])

instance
    ( Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Random (Lng a u) where
    randomR (Lng lo, Lng hi) g =
        (Lng . convert $ z, g')
        where
            (z, g') = qRandomR g lo hi

    random = randomR (Lng $ convert [u| - 180 deg |], Lng $ convert [u| 180 deg |])

qRandomR
    :: (Real a, Fractional a, RandomGen g)
    => g
    -> Quantity a [u| rad |]
    -> Quantity a [u| rad |]
    -> (Quantity a [u| rad |], g)
qRandomR g (MkQuantity lo) (MkQuantity hi) =
    (z, g')
    where
        (n, g') = next g
        (a, b) = genRange g

        dn :: Double
        dn = realToFrac hi - realToFrac lo

        dd :: Double
        dd = fromIntegral (b - a + 1)

        scale :: Double
        scale = dn / dd

        y :: Quantity Double [u| rad |]
        y = MkQuantity $ scale * fromIntegral (n - a) + realToFrac lo

        z :: Quantity _ [u| rad |]
        z = fromRational' . toRational' $ y

instance
    ( Monad m
    , Serial m a
    , Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Serial m (Lat a u) where
    series = series >>- \x -> guard (x >= minBound && x <= maxBound) >> return x

instance
    ( Monad m
    , Serial m a
    , Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Serial m (Lng a u) where
    series = series >>- \x -> guard (x >= minBound && x <= maxBound) >> return x

instance
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => SC.Serial m (LatLng a u) where
    series = decDepth $ LatLng <$> cons2 (\lat lng -> (lat, lng))

instance
    ( Real a
    , Fractional a
    , Arbitrary a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => QC.Arbitrary (LatLng a u) where
    arbitrary = LatLng <$> do
        lat <- arbitraryBoundedRandom
        lng <- arbitraryBoundedRandom
        return (lat, lng)
