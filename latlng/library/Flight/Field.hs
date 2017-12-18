module Flight.Field (FieldOrdering(..)) where

import Data.String (IsString())

class FieldOrdering b where
    fieldOrder :: (Ord a, IsString a) => b -> (a -> a -> Ordering)
