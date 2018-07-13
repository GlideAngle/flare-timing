module Flight.Zone.Radius
    ( Radius(..)
    , QRadius
    , RawRadius
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

type RawRadius = Radius
type QRadius a u = Radius (Quantity a u)

newtype Radius a = Radius a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| m |])
    => DefaultDecimalPlaces (RawRadius q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| m |])
    => Newtype (RawRadius q) q where
    pack = Radius
    unpack (Radius a) = a

instance (q ~ Quantity Double [u| m |]) => ToJSON (Radius q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| m |]) => FromJSON (Radius q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
