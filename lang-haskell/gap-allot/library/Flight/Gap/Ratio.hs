module Flight.Gap.Ratio (isNormal, isFoldNormal) where

import Data.Ratio ((%))

class Num a => Normal a where
    isNormal :: a -> Bool

class (Normal b, Foldable m) => FoldNormal m a b where
    isFoldNormal :: (a -> b -> b) -> b -> m a -> Bool
    isFoldNormal f y xs = isNormal $ foldr f y xs

instance (Num a, Real a) => Normal a where
    isNormal x = let y = toRational x in y >= (0 % 1) && y <= (1 % 1)

instance (Normal b) => FoldNormal [] a b where
    isFoldNormal f y xs = isNormal $ foldr f y xs
