module Flight.Gap.Distance.Min (MinimumDistance(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype MinimumDistance a = MinimumDistance a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (MinimumDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (MinimumDistance q) q where
    pack = MinimumDistance
    unpack (MinimumDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (MinimumDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (MinimumDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
