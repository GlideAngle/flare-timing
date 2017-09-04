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

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Radius(..)
    , Incline(..)
    , Bearing(..)
    , Zone(..)
    )

newtype HaversineTest = HaversineTest (LatLng [u| rad |], LatLng [u| rad |]) deriving Show
newtype ZoneTest = ZoneTest Zone deriving Show
newtype ZonesTest = ZonesTest [Zone] deriving Show

instance Monad m => SC.Serial m HaversineTest where
    series = decDepth $ HaversineTest <$>
        cons4 (\xlat xlng ylat ylng ->
            ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
            , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
            ))

instance Monad m => SC.Serial m ZoneTest where
    series = decDepth $ ZoneTest <$>
        cons2 (\lat lng ->
            Point (LatLng (Lat $ MkQuantity lat, Lng $ MkQuantity lng)))

        \/ cons3 (\lat lng b ->
            Vector
                (Bearing $ MkQuantity b)
                (LatLng (Lat $ MkQuantity lat, Lng $ MkQuantity lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            Cylinder
                (Radius $ MkQuantity r)
                (LatLng (Lat $ MkQuantity lat, Lng $ MkQuantity lng)))

        \/ cons4 (\lat lng (SC.Positive r) i ->
            Conical
                (Incline $ MkQuantity i)
                (Radius $ MkQuantity r)
                (LatLng (Lat $ MkQuantity lat, Lng $ MkQuantity lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            Line
                (Radius $ MkQuantity r)
                (LatLng (Lat $ MkQuantity lat, Lng $ MkQuantity lng)))

        \/ cons3 (\lat lng (SC.Positive r) ->
            SemiCircle
                (Radius $ MkQuantity r)
                (LatLng (Lat $ MkQuantity lat, Lng $ MkQuantity lng)))

instance Monad m => SC.Serial m ZonesTest where
    series = decDepth $ cons1 (\xs -> ZonesTest $ (\(ZoneTest x) -> x) <$> xs)

instance QC.Arbitrary HaversineTest where
    arbitrary = HaversineTest <$> do
        xlat <- arbitrary
        xlng <- arbitrary
        ylat <- arbitrary
        ylng <- arbitrary
        return ( LatLng (Lat $ MkQuantity xlat, Lng $ MkQuantity xlng)
               , LatLng (Lat $ MkQuantity ylat, Lng $ MkQuantity ylng)
               )

instance QC.Arbitrary ZoneTest where
    arbitrary = ZoneTest <$> do
        lat <- arbitrary
        lng <- arbitrary
        let ll = LatLng (Lat $ MkQuantity lat, Lng $ MkQuantity lng)

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

instance QC.Arbitrary ZonesTest where
    arbitrary = do
        len <- choose (2, 4)
        xs <- vector len
        return $ ZonesTest ((\(ZoneTest x) -> x) <$> xs)
