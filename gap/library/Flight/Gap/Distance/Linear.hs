{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Flight.Gap.Distance.Linear
    ( PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    , bestDistance'
    ) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Ratio (pattern (:%))
import Flight.Units ()
import Flight.Gap.Distance.Best (BestDistance(..))
import Flight.Gap.Distance.Pilot (PilotDistance(..))

newtype LinearFraction = LinearFraction Rational deriving (Eq, Ord, Show)

-- | The linear fraction for distance.
linearFraction
    :: BestDistance (Quantity Double [u| km |])
    -> PilotDistance (Quantity Double [u| km |])
    -> LinearFraction
linearFraction (BestDistance bd) (PilotDistance pd) =
    LinearFraction $ (np * db) % (dp * nb)
    where
        MkQuantity (nb :% db) = toRational' bd
        MkQuantity (np :% dp) = toRational' pd

bestDistance' :: Ord a => [PilotDistance a] -> Maybe (BestDistance a)
bestDistance' [] = Nothing
bestDistance' xs = let PilotDistance x = maximum xs in Just . BestDistance $ x
