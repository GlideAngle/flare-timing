{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.LatLng.Family  where

import System.Random
import Data.UnitsOfMeasure (u, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()

-- SEE: https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html
type family F a :: Bool where
    F Double = 'True
    F Float = 'True
    F Rational = 'True
    F a = 'False

type DegToRad a = Quantity a [u| deg |] -> Quantity a [u| rad |]
type RadToDeg a = Quantity a [u| rad |] -> Quantity a [u| deg |]

qRandomR
    :: forall a g
    .  (Real a, Fractional a, RandomGen g)
    => g
    -> Quantity a [u| rad |]
    -> Quantity a [u| rad |]
    -> (Quantity a [u| rad |], g)
qRandomR g (MkQuantity lo) (MkQuantity hi) =
    (z, g')
    where
        lo', hi' :: Double
        lo' = realToFrac lo
        hi' = realToFrac hi

        (x, g') = uniformR (lo', hi') g

        y :: Quantity Double [u| rad |]
        y = MkQuantity x

        z :: Quantity _ [u| rad |]
        z = fromRational' . toRational' $ y