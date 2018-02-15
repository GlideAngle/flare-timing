{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# lANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Props.Haversine (HaversineTest(..)) where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))

newtype HaversineTest a =
    HaversineTest (LatLng a [u| rad |], LatLng a [u| rad |])

deriving instance Show (LatLng a [u| rad |]) => Show (HaversineTest a)

instance (Monad m, SC.Serial m a) => SC.Serial m (HaversineTest a) where
    series = decDepth $ HaversineTest <$>
        cons4 (\xlat xlng ylat ylng ->
            ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
            , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
            ))

instance Arbitrary a => QC.Arbitrary (HaversineTest a) where
    arbitrary = HaversineTest <$> do
        xlat <- arbitrary
        xlng <- arbitrary
        ylat <- arbitrary
        ylng <- arbitrary
        return ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
               , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
               )
