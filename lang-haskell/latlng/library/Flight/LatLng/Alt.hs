module Flight.LatLng.Alt (QAlt, Alt(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

type QAlt a u = Alt (Quantity a u)
newtype Alt a = Alt a deriving (Eq, Ord, Show, Generic)

instance
    (q ~ Quantity Double [u| m |])
    => DefaultDecimalPlaces (Alt q) where
    defdp _ = DecimalPlaces 3

instance
    (q ~ Quantity Double [u| m |])
    => Newtype (Alt q) q where
    pack = Alt
    unpack (Alt a) = a

instance (q ~ Quantity Double [u| m |]) => ToJSON (Alt q) where
    toJSON (Alt x) = toJSON $ ViaQ (Alt y)
        where
            y :: Quantity Double [u| m |]
            y = convert x

instance (q ~ Quantity Double [u| m |]) => FromJSON (Alt q) where
    parseJSON o = do
        ViaQ (Alt x) <- parseJSON o
        return (Alt $ convert x)
