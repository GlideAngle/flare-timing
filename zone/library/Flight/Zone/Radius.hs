module Flight.Zone.Radius (Radius(..)) where

import Data.UnitsOfMeasure.Internal (Quantity(..))

newtype Radius a u = Radius (Quantity a u)
    deriving (Eq, Ord)
