{-# lANGUAGE PatternSynonyms #-}
{-# lANGUAGE ViewPatterns #-}
module Flight.Ratio (pattern (:%), isNormal) where

import Data.Ratio ((%), numerator, denominator)

-- | SEE: http://stackoverflow.com/questions/33325370/why-cant-i-pattern-match-against-a-ratio-in-haskell
pattern num :% denom <- (\x -> (numerator x, denominator x) -> (num, denom))

isNormal :: Rational -> Bool
isNormal x = x >= (0 % 1) && x <= (1 % 1)
