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

import Control.Monad (guard)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))

newtype VincentyTest a =
    VincentyTest (LatLng a [u| rad |], LatLng a [u| rad |])

deriving instance Show (LatLng a [u| rad |]) => Show (VincentyTest a)

instance
    ( Monad m
    , Serial m a
    , Real a
    , Fractional a
    , Convertible u [u| deg |]
    )
    => Serial m (Lat a u) where
    series = series >>- \x -> guard (x >= minBound && x <= maxBound) >> return x

instance
    ( Monad m
    , Serial m a
    , Real a
    , Fractional a
    , Convertible u [u| deg |]
    )
    => Serial m (Lng a u) where
    series = series >>- \x -> guard (x >= minBound && x <= maxBound) >> return x

instance
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    )
    => SC.Serial m (VincentyTest a) where
    series = decDepth $ VincentyTest <$>
        cons4 (\xlat xlng ylat ylng ->
            ( LatLng (xlat, xlng)
            , LatLng (ylat, ylng)
            ))

instance
    (Real a, Fractional a, Arbitrary a)
    => QC.Arbitrary (VincentyTest a) where
    arbitrary = VincentyTest <$> do
        xlat <- arbitraryBoundedRandom
        xlng <- arbitraryBoundedRandom
        ylat <- arbitraryBoundedRandom
        ylng <- arbitraryBoundedRandom
        return ( LatLng (xlat, xlng)
               , LatLng (ylat, ylng)
               )
