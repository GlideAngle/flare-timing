module Flight.Gap.Leading.Area
    ( LeadingAreas(..)
    , LeadingArea(..)
    , LeadingAreaUnits
    , zeroLeadingAreaUnits
    ) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Csv (ToField(..), FromField(..))
import Data.UnitsOfMeasure ((*:), u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

data LeadingAreas a b =
    LeadingAreas
        { areaFlown :: a
        , areaAfterLanding :: b
        , areaBeforeStart :: b
        }
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

deriving instance (Show a, Show b) => Show (LeadingAreas a b)

type LeadingAreaUnits = Quantity Double [u| (km^2)*s |]

zeroLeadingAreaUnits :: LeadingAreaUnits
zeroLeadingAreaUnits =
    (zero :: Quantity _ [u| km |])
    *:
    (zero :: Quantity _ [u| km |])
    *:
    (zero :: Quantity _ [u| s |])

newtype LeadingArea a = LeadingArea a
    deriving (Eq, Ord, Show, Generic)

instance
    (q ~ LeadingAreaUnits)
    => DefaultDecimalPlaces (LeadingArea q) where
    defdp _ = DecimalPlaces 4

instance
    (q ~ LeadingAreaUnits)
    => Newtype (LeadingArea q) q where
    pack = LeadingArea
    unpack (LeadingArea a) = a

instance (q ~ LeadingAreaUnits) => ToJSON (LeadingArea q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ LeadingAreaUnits) => FromJSON (LeadingArea q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

instance
    (q ~ LeadingAreaUnits)
      => ToField (LeadingArea q) where
    toField (LeadingArea (MkQuantity x)) = toField x

instance
    (q ~ LeadingAreaUnits)
      => FromField (LeadingArea q) where
    parseField x = LeadingArea . MkQuantity <$> parseField x
