{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# lANGUAGE PatternSynonyms #-}
module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC

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
        cons2 (\lat lng -> Point (LatLng (lat, lng)))
        \/ cons3 (\lat lng b -> Vector (Bearing b) (LatLng (lat, lng)))
        \/ cons3 (\lat lng r -> Cylinder (Radius r) (LatLng (lat, lng)))
        \/ cons4 (\lat lng r i -> Conical (Incline i) (Radius r) (LatLng (lat, lng)))
        \/ cons3 (\lat lng r -> Line (Radius r) (LatLng (lat, lng)))
        \/ cons3 (\lat lng r -> SemiCircle (Radius r) (LatLng (lat, lng)))

instance Monad m => SC.Serial m ZonesTest where
    series = cons1 (\xs -> ZonesTest $ (\(ZoneTest x) -> x) <$> xs)

instance QC.Arbitrary ZoneTest where
    arbitrary = ZoneTest <$>
        QC.oneof
            [ do
                lat <- arbitrary
                lng <- arbitrary
                return $ Point (LatLng (lat, lng))
            , do
                lat <- arbitrary
                lng <- arbitrary
                b <- arbitrary
                return $ Vector (Bearing b) (LatLng (lat, lng))
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                return $ Cylinder (Radius r) (LatLng (lat, lng))
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                i <- arbitrary
                return $ Conical (Incline i) (Radius r) (LatLng (lat, lng))
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                return $ Line (Radius r) (LatLng (lat, lng))
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                return $ SemiCircle (Radius r) (LatLng (lat, lng))
            ]

instance QC.Arbitrary ZonesTest where
    arbitrary = ZonesTest <$> do
        xs <- arbitrary
        return $ (\(ZoneTest x) -> x) <$> xs
