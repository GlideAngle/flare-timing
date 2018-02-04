{-# LANGUAGE FlexibleContexts #-}

module Flight.Zone.Radius (Radius(..)) where

import Data.UnitsOfMeasure (KnownUnit, Unpack)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Flight.Zone.Raw.Radius

newtype Radius a u = Radius (Quantity a u)
    deriving (Eq, Ord)

instance (Show a, KnownUnit (Unpack u)) => Show (Radius a u) where
    show (Radius q) = show (RawRadius q)
