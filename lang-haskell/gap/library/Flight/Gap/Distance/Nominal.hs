module Flight.Gap.Distance.Nominal (NominalDistance(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype NominalDistance a = NominalDistance a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (NominalDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (NominalDistance q) q where
    pack = NominalDistance
    unpack (NominalDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (NominalDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (NominalDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
