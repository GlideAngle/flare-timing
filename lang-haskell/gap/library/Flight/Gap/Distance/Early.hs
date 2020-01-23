module Flight.Gap.Distance.Early (LaunchToSss(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype LaunchToSss a = LaunchToSss a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (LaunchToSss q) where
    defdp _ = DecimalPlaces 3

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (LaunchToSss q) q where
    pack = LaunchToSss
    unpack (LaunchToSss a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (LaunchToSss q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (LaunchToSss q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
