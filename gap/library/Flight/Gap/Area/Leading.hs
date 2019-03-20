module Flight.Gap.Area.Leading (LeadingArea(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Csv (ToField(..), FromField(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype LeadingArea a = LeadingArea a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| (km^2)*s |])
    => DefaultDecimalPlaces (LeadingArea q) where
    defdp _ = DecimalPlaces 8

instance
    (q ~ Quantity Double [u| (km^2)*s |])
    => Newtype (LeadingArea q) q where
    pack = LeadingArea
    unpack (LeadingArea a) = a

instance (q ~ Quantity Double [u| (km^2)*s |]) => ToJSON (LeadingArea q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| (km^2)*s |]) => FromJSON (LeadingArea q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

instance
    (q ~ Quantity Double [u| (km^2)*s |])
      => ToField (LeadingArea q) where
    toField (LeadingArea (MkQuantity x)) = toField x

instance
    (q ~ Quantity Double [u| (km^2)*s |])
      => FromField (LeadingArea q) where
    parseField x = LeadingArea . MkQuantity <$> parseField x
