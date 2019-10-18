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
    :: (Real a, Fractional a, RandomGen g)
    => g
    -> Quantity a [u| rad |]
    -> Quantity a [u| rad |]
    -> (Quantity a [u| rad |], g)
qRandomR g (MkQuantity lo) (MkQuantity hi) =
    (z, g')
    where
        (n, g') = next g
        (a, b) = genRange g

        dn :: Double
        dn = realToFrac hi - realToFrac lo

        dd :: Double
        dd = fromIntegral (b - a + 1)

        scale :: Double
        scale = dn / dd

        y :: Quantity Double [u| rad |]
        y = MkQuantity $ scale * fromIntegral (n - a) + realToFrac lo

        z :: Quantity _ [u| rad |]
        z = fromRational' . toRational' $ y
