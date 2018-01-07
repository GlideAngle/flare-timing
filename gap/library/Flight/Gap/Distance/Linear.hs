{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Flight.Gap.Distance.Linear
    ( BestDistance(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    ) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Ratio (pattern (:%))
import Flight.Units ()

newtype LinearFraction = LinearFraction Rational deriving (Eq, Ord, Show)

newtype BestDistance = BestDistance (Quantity Double [u| km |])
    deriving (Eq, Ord, Show)

newtype PilotDistance a = PilotDistance a deriving (Eq, Ord, Show)

-- | The linear fraction for distance.
linearFraction
    :: BestDistance
    -> PilotDistance (Quantity Double [u| km |]) 
    -> LinearFraction
linearFraction (BestDistance bd) (PilotDistance pd) =
    LinearFraction $ (np * db) % (dp * nb)
    where
        MkQuantity (nb :% db) = toRational' bd
        MkQuantity (np :% dp) = toRational' pd
