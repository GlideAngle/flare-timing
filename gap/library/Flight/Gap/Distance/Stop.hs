module Flight.Gap.Distance.Stop
    ( LaunchToEss(..)
    , FlownMean(..)
    , FlownStdDev(..)
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype LaunchToEss a = LaunchToEss a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (LaunchToEss q) where
    defdp _ = DecimalPlaces 3

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (LaunchToEss q) q where
    pack = LaunchToEss
    unpack (LaunchToEss a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (LaunchToEss q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (LaunchToEss q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

newtype FlownMean a = FlownMean a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (FlownMean q) where
    defdp _ = DecimalPlaces 3

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (FlownMean q) q where
    pack = FlownMean
    unpack (FlownMean a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (FlownMean q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (FlownMean q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

newtype FlownStdDev a = FlownStdDev a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (FlownStdDev q) where
    defdp _ = DecimalPlaces 3

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (FlownStdDev q) q where
    pack = FlownStdDev
    unpack (FlownStdDev a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (FlownStdDev q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (FlownStdDev q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
