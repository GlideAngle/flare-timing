module Flight.Gap.Weight.GoalRatio (GoalRatio(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

-- | Pilots in goal versus pilots flying.
newtype GoalRatio = GoalRatio Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype GoalRatio Rational where
    pack = GoalRatio
    unpack (GoalRatio a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''GoalRatio
deriveJsonViaSci ''GoalRatio
