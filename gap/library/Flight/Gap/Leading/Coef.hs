module Flight.Gap.Leading.Coef
    ( LeadingCoef(..)
    , LeadingCoefUnits
    , zeroLeadingCoefUnits
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Csv (ToField(..), FromField(..))
import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

type LeadingCoefUnits = Quantity Double [u| 1 |]

zeroLeadingCoefUnits :: LeadingCoefUnits
zeroLeadingCoefUnits = zero

newtype LeadingCoef a = LeadingCoef a
    deriving (Eq, Ord, Show)

instance
    (q ~ LeadingCoefUnits)
    => DefaultDecimalPlaces (LeadingCoef q) where
    defdp _ = DecimalPlaces 8

instance
    (q ~ LeadingCoefUnits)
    => Newtype (LeadingCoef q) q where
    pack = LeadingCoef
    unpack (LeadingCoef a) = a

instance (q ~ LeadingCoefUnits) => ToJSON (LeadingCoef q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ LeadingCoefUnits) => FromJSON (LeadingCoef q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

instance
    (q ~ LeadingCoefUnits)
      => ToField (LeadingCoef q) where
    toField (LeadingCoef (MkQuantity x)) = toField x

instance
    (q ~ LeadingCoefUnits)
      => FromField (LeadingCoef q) where
    parseField x = LeadingCoef . MkQuantity <$> parseField x
