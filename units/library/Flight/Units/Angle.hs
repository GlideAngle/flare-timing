{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Units.Angle (Angle(..), halfPi, deg90, deg270) where

import Data.Fixed (mod')
import Data.UnitsOfMeasure ((+:), (*:), u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

[u| rad |]
[u| deg = (5030569068109113 % 288230376151711744) rad |]
[u| dms = (5030569068109113 % 288230376151711744) rad |]

halfPi :: (Real a, Fractional a) => Quantity a [u| rad |]
halfPi = convert [u| 90 deg |]

deg90 :: (Real a, Fractional a) => Quantity a [u| rad |]
deg90 = halfPi

deg270 :: (Real a, Fractional a) => Quantity a [u| rad |]
deg270 = 3 *: halfPi

class Angle a where
    normalize :: a -> a
    rotate :: a -> a -> a
    fromQuantity :: Convertible u [u| deg |] => Quantity Double u -> a
    toQuantity :: Convertible u [u| deg |] => a -> Quantity Double u

-- |
-- >>> normalize [u| 0.0 deg |]
-- [u| 0.0 deg |]
--
-- >>> normalize [u| 180.0 deg |]
-- [u| 180.0 deg |]
--
-- >>> normalize [u| 1.0 deg |]
-- [u| 1.0 deg |]
--
-- >>> normalize [u| -180.0 deg |]
-- [u| 180.0 deg |]
--
-- >>> normalize [u| -190.0 deg |]
-- [u| 170.0 deg |]
--
-- >>> normalize [u| -170.0 deg |]
-- [u| 190.0 deg |]
--
-- >>> normalize [u| -190.93544548 deg |]
-- [u| 169.06455452 deg |]
--
-- >>> normalize [u| -169.06455452 deg |]
-- [u| 190.93544548 deg |]
instance Convertible u [u| deg |] => Angle (Quantity Double u) where
    normalize d' =
        convert n
        where
            n :: Quantity Double [u| deg |]
            n = MkQuantity $ d `mod'` 360.0

            (MkQuantity d) = convert d' :: Quantity Double [u| deg |]

    rotate rotation d' =
        normalize . convert $ d +: r
        where
            r :: Quantity Double [u| deg |]
            r = convert rotation

            d :: Quantity Double [u| deg |]
            d = convert d'

    fromQuantity = convert
    toQuantity = convert

instance Convertible u [u| deg |] => Angle (Quantity Rational u) where
    normalize d' =
        convert n
        where
            n :: Quantity Rational [u| deg |]
            n = MkQuantity $ d `mod'` 360.0

            (MkQuantity d) = convert d' :: Quantity Rational [u| deg |]

    rotate rotation d' =
        normalize . convert $ d +: r
        where
            r :: Quantity Rational [u| deg |]
            r = convert rotation

            d :: Quantity Rational [u| deg |]
            d = convert d'

    fromQuantity = toRational' . convert
    toQuantity = convert . fromRational'
