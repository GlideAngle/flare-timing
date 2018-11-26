module Flight.Gap.Points.Task (TaskPlacing(..), TaskPoints(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TaskPlacing = TaskPlacing Integer
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype TaskPoints = TaskPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype TaskPoints Rational where
    pack = TaskPoints
    unpack (TaskPoints a) = a

deriveDecimalPlaces (DecimalPlaces 0) ''TaskPoints
deriveJsonViaSci ''TaskPoints
