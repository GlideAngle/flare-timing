module Flight.Zone.Incline (QIncline, Incline(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

type QIncline a u = Incline (Quantity a u)

-- | An incline such as the incline component of a conical zone.
newtype Incline a = Incline a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| rad |])
    => DefaultDecimalPlaces (Incline q) where
    defdp _ = DecimalPlaces 2

instance
    (q ~ Quantity Double [u| rad |])
    => Newtype (Incline q) q where
    pack = Incline
    unpack (Incline a) = a

instance (q ~ Quantity Double [u| rad |]) => ToJSON (Incline q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| rad |]) => FromJSON (Incline q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
