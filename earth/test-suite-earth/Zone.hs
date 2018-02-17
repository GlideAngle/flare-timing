{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Zone
    ( QLL, MkZone
    , point, vector, cylinder, line, semicircle
    , describedZones
    , showQ
    ) where

import Prelude hiding (span)
import Data.UnitsOfMeasure (u, zero, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , Incline (..)
    , Bearing(..)
    )
import DegMinSec (fromQ)

type QLL a = (Quantity a [u| rad |], Quantity a [u| rad |])
type MkZone a = Real a => Radius Rational [u| m |] -> QLL a -> Zone Rational

showQ :: QLL Double -> String
showQ (x, y) =
    show (fromQ x, fromQ y)

toLL :: Real a => QLL a -> LatLng Rational [u| rad |]
toLL (lat, lng) =
    LatLng (Lat lat', Lng lng')
    where
        lat' = toRational' lat
        lng' = toRational' lng

point :: MkZone a
point _ x = Point $ toLL x

vector :: MkZone a
vector _ x = Vector (Bearing zero) (toLL x) 

cylinder :: MkZone a
cylinder r x = Cylinder r (toLL x)

conical :: MkZone a
conical r x = Conical (Incline $ MkQuantity 1) r (toLL x)

line :: MkZone a
line r x = Line r (toLL x) 

semicircle :: MkZone a
semicircle r x = SemiCircle r (toLL x)

describedZones
    :: Real a
    =>
        [
            ( String
            , Radius Rational [u| m |] -> QLL a -> Zone Rational
            )
        ]
describedZones =
    [ ("point", point)
    , ("vector", vector)
    , ("cylinder", cylinder)
    , ("conical", conical)
    , ("line", line)
    , ("semicircle", semicircle)
    ]

