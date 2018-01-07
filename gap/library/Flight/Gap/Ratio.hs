{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Flight.Gap.Ratio (pattern (:%), isNormal, isFoldNormal) where

import Data.Ratio (Ratio, (%), numerator, denominator)

-- | SEE: http://stackoverflow.com/questions/33325370/why-cant-i-pattern-match-against-a-ratio-in-haskell
pattern (:%) :: forall t. t -> t -> Ratio t
pattern num :% denom <- (\x -> (numerator x, denominator x) -> (num, denom))

class Num a => Normal a where
    isNormal :: a -> Bool

class (Normal b, Foldable m) => FoldNormal m a b where
    isFoldNormal :: (a -> b -> b) -> b -> m a -> Bool
    isFoldNormal f y xs = isNormal $ foldr f y xs

instance Normal Rational where
    isNormal x = x >= (0 % 1) && x <= (1 % 1)

instance (Normal b) => FoldNormal [] a b where
    isFoldNormal f y xs = isNormal $ foldr f y xs
