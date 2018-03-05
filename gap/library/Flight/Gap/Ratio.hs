{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Ratio (isNormal, isFoldNormal) where

import Data.Ratio ((%))

class Num a => Normal a where
    isNormal :: a -> Bool

class (Normal b, Foldable m) => FoldNormal m a b where
    isFoldNormal :: (a -> b -> b) -> b -> m a -> Bool
    isFoldNormal f y xs = isNormal $ foldr f y xs

instance Normal Rational where
    isNormal x = x >= (0 % 1) && x <= (1 % 1)

instance (Normal b) => FoldNormal [] a b where
    isFoldNormal f y xs = isNormal $ foldr f y xs
