module Flight.Gap.Leading.Coef
    ( LeadingCoef(..)
    , LeadingCoefUnits
    , zeroLeadingCoefUnits
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Control.Applicative (empty)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(Number))
import Data.Csv (ToField(..), FromField(..))
import Data.UnitsOfMeasure (u, zero, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), fromSci, toSci)

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
    toJSON x@(LeadingCoef q) =
        let c :: Double
            c = unQuantity q

        in Number $ toSci (defdp x) (toRational c)

instance (q ~ LeadingCoefUnits) => FromJSON (LeadingCoef q) where
    parseJSON x@(Number _) =
        LeadingCoef . MkQuantity . fromRational . fromSci
        <$> parseJSON x

    parseJSON _ = empty

instance
    (q ~ LeadingCoefUnits)
      => ToField (LeadingCoef q) where
    toField (LeadingCoef (MkQuantity x)) = toField x

instance
    (q ~ LeadingCoefUnits)
      => FromField (LeadingCoef q) where
    parseField x = LeadingCoef . MkQuantity <$> parseField x
