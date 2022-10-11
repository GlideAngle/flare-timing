{-# LANGUAGE ViewPatterns #-}

module Flight.Ratio (pattern (:%), splitRatio) where

import Data.Ratio (Ratio, numerator, denominator)

-- | SEE: http://stackoverflow.com/questions/33325370/why-cant-i-pattern-match-against-a-ratio-in-haskell
pattern (:%) :: forall a. a -> a -> Ratio a
pattern n :% d <- (splitRatio -> (n, d))
{-# COMPLETE (:%) #-}

splitRatio :: Ratio a -> (a, a)
splitRatio x = (numerator x, denominator x)
