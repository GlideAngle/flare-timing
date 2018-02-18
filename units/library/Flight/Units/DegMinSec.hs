{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Units.DegMinSec
    ( DMS(..)
    , toDeg
    , toQDeg
    , toQRad
    , fromQ
    ) where

import Prelude hiding (min)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.Units ()

newtype DMS = DMS (Int, Int, Double)

instance Show DMS where
    show = showDMS

showDMS :: DMS -> String
showDMS (DMS (deg, 0, 0)) =
    show deg ++ "°"
showDMS (DMS (deg, min, 0)) =
    show deg ++ "°" ++ show min ++ "'"
showDMS (DMS (deg, min, sec)) =
    if fromIntegral isec == sec then
        show deg ++ "°" ++ show min ++ "'" ++ show isec ++ "''"
    else
        show deg ++ "°" ++ show min ++ "'" ++ show sec ++ "''"
    where
        isec :: Int
        isec = floor sec

toDeg :: DMS -> Double
toDeg (DMS (deg, min, s)) =
    signum d * (abs d + m / 60 + s / 3600)
        where
            d = fromIntegral deg
            m = fromIntegral min 

toQDeg :: DMS -> Quantity Double [u| deg |]
toQDeg dms =
    MkQuantity . toDeg $ dms

toQRad :: DMS -> Quantity Double [u| rad |]
toQRad dms =
    convert . toQDeg $ dms

fromQ :: Convertible u [u| deg |] => Quantity Double u -> DMS
fromQ q' =
    DMS (s * dd, mm, fromIntegral ss)
    where
        MkQuantity d = convert q' :: Quantity Double [u| deg |]

        totalSecs :: Int
        totalSecs = round $ 3600.0 * d

        s = signum totalSecs

        (dd, ms) = quotRem totalSecs 3600
        (mm, ss) = quotRem ms 60
