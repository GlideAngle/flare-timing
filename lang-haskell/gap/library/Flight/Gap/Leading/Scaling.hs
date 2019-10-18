module Flight.Gap.Leading.Scaling
    ( LeadingAreaScaling(..)
    , AreaToCoef(..)
    , AreaToCoefUnits
    , CoefUnits
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()

newtype LeadingAreaScaling = LeadingAreaScaling Rational
    deriving (Eq, Ord, Show)

type AreaToCoefUnits = Quantity Rational [u| 1/((km^2)*s) |]
type CoefUnits = Quantity Rational [u| 1 |]

newtype AreaToCoef a = AreaToCoef a
    deriving (Eq, Ord, Show)

instance
    (q ~ AreaToCoefUnits)
    => DefaultDecimalPlaces (AreaToCoef q) where
    defdp _ = DecimalPlaces 14

instance
    (q ~ AreaToCoefUnits)
    => Newtype (AreaToCoef q) q where
    pack = AreaToCoef
    unpack (AreaToCoef a) = a

instance (q ~ AreaToCoefUnits) => ToJSON (AreaToCoef q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ AreaToCoefUnits) => FromJSON (AreaToCoef q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
