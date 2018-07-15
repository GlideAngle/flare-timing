module Flight.LatLng.Lng (QLng, Lng(..)) where

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

type QLng a u = Lng (Quantity a u)
newtype Lng a = Lng a deriving (Eq, Ord)

instance
    (q ~ Quantity Double [u| deg |])
    => DefaultDecimalPlaces (Lng q) where
    defdp _ = DecimalPlaces 8

instance
    (q ~ Quantity Double [u| deg |])
    => Newtype (Lng q) q where
    pack = Lng
    unpack (Lng a) = a

instance (q ~ Quantity Double [u| rad |]) => ToJSON (Lng q) where
    toJSON (Lng x) = toJSON $ ViaQ (Lng y)
        where
            y :: Quantity Double [u| deg |]
            y = convert x

instance (q ~ Quantity Double [u| rad |]) => FromJSON (Lng q) where
    parseJSON o = do
        ViaQ (Lng x) <- parseJSON o
        return (Lng $ convert x)

instance
    (F a ~ flag, ShowLng flag a u, KnownUnit (Unpack u))
    => Show (QLng a u) where
    show = showLng (Proxy :: Proxy flag)

class (KnownUnit (Unpack u)) => ShowLng (flag :: Bool) a u where
    showLng :: Proxy flag -> QLng a u -> String

instance
    ( KnownUnit (Unpack u)
    , Convertible u [u| deg |]
    )
    => ShowLng 'True Double u where
    showLng _ (Lng lat) = D.showAngle lat

instance (KnownUnit (Unpack u)) => ShowLng 'True Float u where
    showLng _ (Lng lat) = F.showAngle lat

instance (KnownUnit (Unpack u)) => ShowLng 'True Rational u where
    showLng _ (Lng lat) = R.showAngle lat

instance (KnownUnit (Unpack u), Show a) => ShowLng 'False a u where
    showLng _ (Lng lng) = show lng

instance (Fractional a, Convertible u [u| deg |]) => Bounded (QLng a u) where
    minBound = Lng $ convert [u| - 90 deg |]
    maxBound = Lng $ convert [u| 90 deg |]

instance
    ( Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Random (QLng a u) where
    randomR (Lng lo, Lng hi) g =
        (Lng . convert $ z, g')
        where
            (z, g') = qRandomR g lo hi

    random = randomR (Lng $ convert [u| - 180 deg |], Lng $ convert [u| 180 deg |])

instance
    ( Monad m
    , Serial m a
    , Real a
    , Fractional a
    , Convertible u [u| deg |]
    , u ~ [u| rad |]
    )
    => Serial m (QLng a u) where
    series = series >>- \x -> guard (x >= minBound && x <= maxBound) >> return x
