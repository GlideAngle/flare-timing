{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# lANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Defs ()

import Flight.Zone
    ( LatLng(..)
    , Radius(..)
    , Incline(..)
    , Bearing(..)
    , Zone(..)
    , Zone(..)
    )

newtype ZoneTest = ZoneTest Zone deriving Show
newtype ZonesTest = ZonesTest [Zone] deriving Show

instance Monad m => SC.Serial m ZoneTest where
    series = ZoneTest <$>
        cons2 (\lat lng ->
            Point (LatLng (lat, lng)))
        \/ cons3 (\lat lng b ->
            Vector (Bearing b) (LatLng (lat, lng)))
        \/ cons3 (\lat lng (SC.Positive r) ->
            Cylinder (Radius (MkQuantity r)) (LatLng (lat, lng)))
        \/ cons4 (\lat lng (SC.Positive r) i ->
            Conical (Incline i) (Radius (MkQuantity r)) (LatLng (lat, lng)))
        \/ cons3 (\lat lng (SC.Positive r) ->
            Line (Radius (MkQuantity r)) (LatLng (lat, lng)))
        \/ cons3 (\lat lng (SC.Positive r) ->
            SemiCircle (Radius (MkQuantity r)) (LatLng (lat, lng)))

instance Monad m => SC.Serial m ZonesTest where
    series = cons1 (\xs -> ZonesTest $ (\(ZoneTest x) -> x) <$> xs)

instance QC.Arbitrary ZoneTest where
    arbitrary = ZoneTest <$> do
        lat <- arbitrary
        lng <- arbitrary
        let ll = LatLng (lat, lng)

        QC.frequency $
            zip
                [1, 1, 4, 1, 1, 1]
                [ return $ Point ll
                , do
                    (QC.Positive b) <- arbitrary
                    return $ Vector (Bearing b) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ Cylinder (Radius (MkQuantity r)) ll
                , do
                    (QC.Positive r) <- arbitrary
                    i <- arbitrary
                    return $ Conical (Incline i) (Radius (MkQuantity r)) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ Line (Radius (MkQuantity r)) ll
                , do
                    (QC.Positive r) <- arbitrary
                    return $ SemiCircle (Radius (MkQuantity r)) ll
                ]

instance QC.Arbitrary ZonesTest where
    arbitrary = do
        len <- choose (2, 4)
        xs <- vector len
        return $ ZonesTest ((\(ZoneTest x) -> x) <$> xs)
