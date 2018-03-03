{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE OverloadedStrings #-}
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
import Data.Fixed (mod')
import Data.Text.Lazy (unpack)
import Formatting ((%), format)
import qualified Formatting.ShortFormatters as Fmt (sf)
import Data.UnitsOfMeasure ((+:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.Units ()
import Flight.Units.Angle (Angle(..))

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
        show deg
        ++ "°"
        ++ show min
        ++ "'"
        ++ (unpack $ format (Fmt.sf % "''") sec)
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
    DMS (s * dd, mm, ss)
    where
        MkQuantity d = convert q' :: Quantity Double [u| deg |]

        totalSecs :: Int
        totalSecs = round $ 3600.0 * d

        s = signum totalSecs

        (dd, ms) = quotRem (abs totalSecs) 3600
        mm = quot ms 60

        ss =
            ((abs d) - fromIntegral dd) * 3600.0
            - (fromIntegral $ mm * 60)

instance Angle DMS where
    normalize dms =
        fromQuantity n
        where
            n :: Quantity Double [u| deg |]
            n = MkQuantity $ d `mod'` 360.0

            (MkQuantity d) = toQuantity dms :: Quantity Double [u| deg |]

    rotate rotation dms =
        normalize . fromQuantity $ d +: r
        where
            r :: Quantity Double [u| deg |]
            r = toQuantity rotation

            d :: Quantity Double [u| deg |]
            d = toQuantity dms

    fromQuantity = fromQ
    toQuantity = convert . toQDeg
