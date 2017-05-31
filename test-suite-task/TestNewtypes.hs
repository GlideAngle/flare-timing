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
        \/ cons3 (\lat lng b -> Vector (LatLng (lat, lng)) (Bearing b))
        \/ cons3 (\lat lng r -> Cylinder (LatLng (lat, lng)) (Radius r))
        \/ cons4 (\lat lng r i -> Conical (LatLng (lat, lng)) (Radius r) (Incline i))
        \/ cons3 (\lat lng r -> Line (LatLng (lat, lng)) (Radius r))
        \/ cons3 (\lat lng r -> SemiCircle (LatLng (lat, lng)) (Radius r))

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
                return $ Vector (LatLng (lat, lng)) (Bearing b)
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                return $ Cylinder (LatLng (lat, lng)) (Radius r)
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                i <- arbitrary
                return $ Conical (LatLng (lat, lng)) (Radius r) (Incline i)
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                return $ Line (LatLng (lat, lng)) (Radius r)
            , do
                lat <- arbitrary
                lng <- arbitrary
                r <- arbitrary
                return $ SemiCircle (LatLng (lat, lng)) (Radius r)
            ]

instance QC.Arbitrary ZonesTest where
    arbitrary = ZonesTest <$> do
        xs <- arbitrary
        return $ (\(ZoneTest x) -> x) <$> xs
