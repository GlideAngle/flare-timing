module Flight.Gap.Distance.Max (MaximumDistance(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype MaximumDistance a = MaximumDistance a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (MaximumDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (MaximumDistance q) q where
    pack = MaximumDistance
    unpack (MaximumDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (MaximumDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (MaximumDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
