module Flight.Gap.Leading.Area
    ( LeadingAreas(..)
    , LeadingArea(..)
    , LeadingAreaUnits
    , LeadingArea1Units, zeroLeadingArea1Units
    , LeadingArea2Units, zeroLeadingArea2Units
    , LeadingAreaToCoefUnits
    , LeadingArea1ToCoefUnits
    , LeadingArea2ToCoefUnits
    ) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Csv (ToField(..), FromField(..))
import Data.UnitsOfMeasure (KnownUnit, Unpack, (*:), u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

type LeadingAreaUnits u = Quantity Double u
type LeadingAreaToCoefUnits u = Quantity Rational u
type LeadingArea1U = [u| km*s |]
type LeadingArea2U = [u| (km^2)*s |]
type LeadingArea1UI = [u| 1/(km*s) |]
type LeadingArea2UI = [u| 1/((km^2)*s) |]
type LeadingArea1Units = LeadingAreaUnits LeadingArea1U
type LeadingArea2Units = LeadingAreaUnits LeadingArea2U
type LeadingArea1ToCoefUnits = LeadingAreaToCoefUnits LeadingArea1UI
type LeadingArea2ToCoefUnits = LeadingAreaToCoefUnits LeadingArea2UI

zeroLeadingArea1Units :: LeadingArea1Units
zeroLeadingArea1Units =
    (zero :: Quantity _ [u| km |])
    *:
    (zero :: Quantity _ [u| s |])

zeroLeadingArea2Units :: LeadingArea2Units
zeroLeadingArea2Units =
    (zero :: Quantity _ [u| km |])
    *:
    (zero :: Quantity _ [u| km |])
    *:
    (zero :: Quantity _ [u| s |])

data LeadingAreas a b =
    LeadingAreas
        { areaFlown :: a
        , areaAfterLanding :: b
        , areaBeforeStart :: b
        }
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

deriving instance (Show a, Show b) => Show (LeadingAreas a b)

newtype LeadingArea a = LeadingArea a
    deriving (Eq, Ord, Show, Generic)

instance (q ~ LeadingAreaUnits u) => DefaultDecimalPlaces (LeadingArea q) where
    defdp _ = DecimalPlaces 4

instance (q ~ LeadingAreaUnits u) => Newtype (LeadingArea q) q where
    pack = LeadingArea
    unpack (LeadingArea a) = a

instance (KnownUnit (Unpack u), q ~ LeadingAreaUnits u) => ToJSON (LeadingArea q) where
    toJSON x = toJSON $ ViaQ x

instance (KnownUnit (Unpack u), q ~ LeadingAreaUnits u) => FromJSON (LeadingArea q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

instance (q ~ LeadingAreaUnits u) => ToField (LeadingArea q) where
    toField (LeadingArea (MkQuantity x)) = toField x

instance (q ~ LeadingAreaUnits u) => FromField (LeadingArea q) where
    parseField x = LeadingArea . MkQuantity <$> parseField x
