{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Units
    ( abs
    , toRational'
    , showRadian
    ) where

import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Bifunctor.Flip (Flip(..))
import Data.Number.RoundingFunctions (dpRound)

[u| s, m |]

[u| km = 1000 m, min = 60 s, h = 3600 s, d = 86400 s |]

[u| ft = 100 % 328 m, mi = 1609.344 m, mph = mi/h |]

[u| rad |]
[u| deg = (5030569068109113 % 288230376151711744) rad |]

-- | Convert any 'Real' quantity into a 'Rational' type ('toRational').
toRational' :: Real a => Quantity a u -> Quantity Rational u
toRational' (MkQuantity x) = MkQuantity (toRational x)

instance Functor (Flip Quantity u) where
    fmap = map'

map' :: (a -> b) -> Flip Quantity u a -> Flip Quantity u b
map' f (Flip (MkQuantity x)) = Flip $ MkQuantity $ f x

showRadian :: Quantity Rational [u| rad |] -> String
showRadian b = show dbl
    where
        deg = convert b :: Quantity Rational [u| deg |]
        Flip rounded = dpRound 3 <$> Flip deg
        MkQuantity dbl = fromRational' rounded :: Quantity Double [u| deg |]
