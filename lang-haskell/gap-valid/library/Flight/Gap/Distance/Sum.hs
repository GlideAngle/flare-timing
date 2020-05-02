module Flight.Gap.Distance.Sum (SumOfDistance(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype SumOfDistance a = SumOfDistance a
    deriving (Eq, Ord, Show, Generic)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (SumOfDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (SumOfDistance q) q where
    pack = SumOfDistance
    unpack (SumOfDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (SumOfDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (SumOfDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
