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

module Props.Vincenty (VincentyTest(..)) where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)

import Flight.LatLng (LatLng(..))

newtype VincentyTest a =
    VincentyTest (LatLng a [u| rad |], LatLng a [u| rad |])

deriving instance Show (LatLng a [u| rad |]) => Show (VincentyTest a)

instance
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    )
    => SC.Serial m (VincentyTest a) where
    series = decDepth $ VincentyTest <$> series

instance
    (Real a, Fractional a, Arbitrary a)
    => QC.Arbitrary (VincentyTest a) where
    arbitrary = VincentyTest <$> do
        ll <- arbitrary
        return ll
