module Flight.Gap.Time.Pilot (PilotTime(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

-- | Pilot time for the task, units of hours.
newtype PilotTime a = PilotTime a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| h |])
    => DefaultDecimalPlaces (PilotTime q) where
    defdp _ = DecimalPlaces 6

instance
    (q ~ Quantity Double [u| h |])
    => Newtype (PilotTime q) q where
    pack = PilotTime
    unpack (PilotTime a) = a

instance (q ~ Quantity Double [u| h |]) => ToJSON (PilotTime q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| h |]) => FromJSON (PilotTime q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
