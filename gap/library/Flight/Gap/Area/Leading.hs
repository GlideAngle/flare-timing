module Flight.Gap.Area.Leading (LeadingAreaStep(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Csv (ToField(..), FromField(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype LeadingAreaStep a = LeadingAreaStep a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| (km^2)*s |])
    => DefaultDecimalPlaces (LeadingAreaStep q) where
    defdp _ = DecimalPlaces 8

instance
    (q ~ Quantity Double [u| (km^2)*s |])
    => Newtype (LeadingAreaStep q) q where
    pack = LeadingAreaStep
    unpack (LeadingAreaStep a) = a

instance (q ~ Quantity Double [u| (km^2)*s |]) => ToJSON (LeadingAreaStep q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| (km^2)*s |]) => FromJSON (LeadingAreaStep q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

instance
    (q ~ Quantity Double [u| (km^2)*s |])
      => ToField (LeadingAreaStep q) where
    toField (LeadingAreaStep (MkQuantity x)) = toField x

instance
    (q ~ Quantity Double [u| (km^2)*s |])
      => FromField (LeadingAreaStep q) where
    parseField x = LeadingAreaStep . MkQuantity <$> parseField x
