module Flight.LatLng.Lat (QLat, Lat(..)) where

import System.Random
import Data.Proxy
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (KnownUnit, Unpack, u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))
import Control.Monad (guard)
import Test.SmallCheck.Series as SC (Serial(..), (>>-))

import qualified Flight.LatLng.Double as D
import qualified Flight.LatLng.Float as F
import qualified Flight.LatLng.Rational as R
import Flight.LatLng.Family

type QLat a u = Lat (Quantity a u)
newtype Lat a = Lat a deriving (Eq, Ord)

instance
    (q ~ Quantity Double [u| deg |])
    => DefaultDecimalPlaces (Lat q) where
    defdp _ = DecimalPlaces 8

instance
    (q ~ Quantity Double [u| deg |])
    => Newtype (Lat q) q where
    pack = Lat
    unpack (Lat a) = a

instance (q ~ Quantity Double [u| rad |]) => ToJSON (Lat q) where
    toJSON (Lat x) = toJSON $ ViaQ (Lat y)
        where
            y :: Quantity Double [u| deg |]
            y = convert x

instance (q ~ Quantity Double [u| rad |]) => FromJSON (Lat q) where
    parseJSON o = do
        ViaQ (Lat x) <- parseJSON o
        return (Lat $ convert x)

instance
    (F a ~ flag, ShowLat flag a u, KnownUnit (Unpack u))
    => Show (QLat a u) where
    show = showLat (Proxy :: Proxy flag)

class (KnownUnit (Unpack u)) => ShowLat (flag :: Bool) a u where
    showLat :: Proxy flag -> QLat a u -> String

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

instance (KnownUnit (Unpack u), Show a) => ShowLat 'False a u where
    showLat _ (Lat lat) = show lat

instance (Fractional a, Convertible u [u| deg |]) => Bounded (QLat a u) where
    minBound = Lat $ convert [u| - 90 deg |]
    maxBound = Lat $ convert [u| 90 deg |]

instance
    ( Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Random (QLat a u) where
    randomR (Lat lo, Lat hi) g =
        (Lat . convert $ z, g')
        where
            (z, g') = qRandomR g lo hi

    random = randomR (Lat $ convert [u| - 90 deg |], Lat $ convert [u| 90 deg |])

instance
    ( Monad m
    , Serial m a
    , Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Serial m (QLat a u) where
    series = series >>- \x -> guard (x >= minBound && x <= maxBound) >> return x

