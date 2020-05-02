module Flight.Gap.Time.Arrival (ArrivalTime(..), ArrivalLag(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype ArrivalTime a = ArrivalTime a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| h |])
    => DefaultDecimalPlaces (ArrivalTime q) where
    defdp _ = DecimalPlaces 6

instance
    (q ~ Quantity Double [u| h |])
    => Newtype (ArrivalTime q) q where
    pack = ArrivalTime
    unpack (ArrivalTime a) = a

instance (q ~ Quantity Double [u| h |]) => ToJSON (ArrivalTime q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| h |]) => FromJSON (ArrivalTime q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

newtype ArrivalLag a = ArrivalLag a
    deriving (Eq, Ord, Show, Generic)

instance
    (q ~ Quantity Double [u| h |])
    => DefaultDecimalPlaces (ArrivalLag q) where
    defdp _ = DecimalPlaces 6

instance
    (q ~ Quantity Double [u| h |])
    => Newtype (ArrivalLag q) q where
    pack = ArrivalLag
    unpack (ArrivalLag a) = a

instance (q ~ Quantity Double [u| h |]) => ToJSON (ArrivalLag q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| h |]) => FromJSON (ArrivalLag q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
