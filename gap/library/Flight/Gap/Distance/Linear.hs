module Flight.Gap.Distance.Linear
    ( PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    , bestDistance'
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)
import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Ratio (pattern (:%))
import Flight.Units ()
import Flight.Gap.Distance.Stop (FlownMax(..))
import Flight.Gap.Distance.Pilot (PilotDistance(..))

newtype LinearFraction = LinearFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype LinearFraction Rational where
    pack = LinearFraction
    unpack (LinearFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LinearFraction
deriveJsonViaSci ''LinearFraction

-- | The linear fraction for distance.
linearFraction
    :: FlownMax (Quantity Double [u| km |])
    -> PilotDistance (Quantity Double [u| km |])
    -> LinearFraction
linearFraction (FlownMax bd) (PilotDistance pd) =
    LinearFraction $ (np * db) % (dp * nb)
    where
        MkQuantity (nb :% db) = toRational' bd
        MkQuantity (np :% dp) = toRational' pd

bestDistance' :: Ord a => [PilotDistance a] -> Maybe (FlownMax a)
bestDistance' [] = Nothing
bestDistance' xs = let PilotDistance x = maximum xs in Just . FlownMax $ x
