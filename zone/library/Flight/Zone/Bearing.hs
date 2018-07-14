module Flight.Zone.Bearing (QBearing, Bearing(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

type QBearing a u = Bearing (Quantity a u)

-- | A bearing such as the component of a vector zone.
newtype Bearing a = Bearing a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| rad |])
    => DefaultDecimalPlaces (Bearing q) where
    defdp _ = DecimalPlaces 2

instance
    (q ~ Quantity Double [u| rad |])
    => Newtype (Bearing q) q where
    pack = Bearing
    unpack (Bearing a) = a

instance (q ~ Quantity Double [u| rad |]) => ToJSON (Bearing q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| rad |]) => FromJSON (Bearing q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
