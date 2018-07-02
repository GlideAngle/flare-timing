{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Flight.Zone.Radius (Radius(..)) where

import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.Units ()
import Flight.Distance (TaskDistance(..))

newtype Radius a u = Radius (Quantity a u)
    deriving (Eq, Ord)

instance
    ( Real a
    , Fractional a
    , Show a
    , Convertible u [u| m |]
    )
    => Show (Radius a u) where
    show (Radius q) = show . TaskDistance . convert $ q
