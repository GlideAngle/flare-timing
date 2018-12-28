module Flight.Zone.AltTime (QAltTime, AltTime(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

type QAltTime a u = AltTime (Quantity a u)

-- | A time bonus for altitude.
newtype AltTime a = AltTime a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| s / m |])
    => DefaultDecimalPlaces (AltTime q) where
    defdp _ = DecimalPlaces 3

instance
    (q ~ Quantity Double [u| s / m |])
    => Newtype (AltTime q) q where
    pack = AltTime
    unpack (AltTime a) = a

instance (q ~ Quantity Double [u| s / m |]) => ToJSON (AltTime q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| s / m |]) => FromJSON (AltTime q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
