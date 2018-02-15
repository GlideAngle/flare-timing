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

module Ellipsoid.TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Control.Monad (guard)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Convert (Convertible)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone (Radius(..), Incline(..), Bearing(..), Zone(..))

newtype VincentyTest a =
    VincentyTest (LatLng a [u| rad |], LatLng a [u| rad |])

deriving instance Show (LatLng a [u| rad |]) => Show (VincentyTest a)

newtype ZoneTest a = ZoneTest (Zone a)
newtype ZonesTest a = ZonesTest [Zone a]

deriving instance (Real a, Show a, Show (LatLng a [u| rad |])) => Show (ZoneTest a)
deriving instance (Real a, Show a, Show (LatLng a [u| rad |])) => Show (ZonesTest a)

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
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    )
    => SC.Serial m (ZoneTest a) where
    series = decDepth $ ZoneTest <$>
        cons2 (\lat lng ->
            Point (LatLng (lat, lng)))

        \/ cons3 (\lat lng b ->
            Vector
                (Bearing $ MkQuantity b)
                (LatLng (lat, lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            Cylinder
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

        \/ cons4 (\lat lng (SC.Positive r) i ->
            Conical
                (Incline $ MkQuantity i)
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            Line
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            SemiCircle
                (Radius $ MkQuantity r)
                (LatLng (lat, lng)))

instance
    ( Monad m
    , SC.Serial m a
    , Real a
    , Fractional a
    )
    => SC.Serial m (ZonesTest a) where
    series = decDepth $ cons1 (\xs -> ZonesTest $ (\(ZoneTest x) -> x) <$> xs)

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

instance
    (Real a, Fractional a, Arbitrary a)
    => QC.Arbitrary (ZoneTest a) where
    arbitrary = ZoneTest <$> do
        lat <- arbitraryBoundedRandom
        lng <- arbitraryBoundedRandom
        let ll = LatLng (lat, lng)

        QC.frequency $
            zip
                [1, 1, 4, 1, 1, 1]
                [ return $ Point ll
                , do
                    (QC.Positive b) <- arbitrary
                    return $ Vector (Bearing $ MkQuantity b) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ Cylinder (Radius $ MkQuantity r) ll
                , do
                    (QC.Positive r) <- arbitrary
                    (QC.Positive i) <- arbitrary
                    return $ Conical (Incline $ MkQuantity i) (Radius $ MkQuantity r) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ Line (Radius $ MkQuantity r) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ SemiCircle (Radius $ MkQuantity r) ll
                ]

instance
    (Real a, Fractional a, Arbitrary a)
    => QC.Arbitrary (ZonesTest a) where
    arbitrary = do
        len <- choose (2, 4)
        xs <- vector len
        return $ ZonesTest ((\(ZoneTest x) -> x) <$> xs)
