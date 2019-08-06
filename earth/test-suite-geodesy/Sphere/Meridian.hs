module Sphere.Meridian (meridianUnits) where

import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (QRadius, Radius(..))
import Flight.Earth.Sphere (earthRadius)
import Zone (MkZone, QLL, describedZones, showQ)
import qualified Distance as D (DistanceClose, toDistanceClose)
import Sphere.Span (spanD, spanR)

meridianUnits :: TestTree
meridianUnits =
    testGroup "Meridian arc distance tests"
    [ testGroup "With doubles" (uncurry f <$> describedZones)
    , testGroup "With rationals" (uncurry g <$> describedZones)
    ]
    where
        f s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")
                (D.toDistanceClose spanD)
                tolerancesD

        g s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")
                (D.toDistanceClose spanR)
                tolerancesR

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    [ ((z, z), (z, [u| 1 rad |]))
    , ((z, z), (z, [u| -1 rad |]))
    ]
    where
        z = [u| 0 rad |]

distances :: (Real a, Fractional a) => [QRadius a [u| m |]]
distances =
    repeat earthRadius

tolerancesD :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerancesD =
    repeat [u| 0 mm |]

tolerancesR :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerancesR =
    repeat [u| 0 mm |]

distanceMeridian
    :: (Enum a, Real a, Fractional a)
    => String
    -> D.DistanceClose a
    -> [Quantity a [u| mm |]]
    -> MkZone a a
    -> TestTree
distanceMeridian s f tolerances g =
    testGroup s
    $ zipWith3
        (\tolerance r@(Radius r') (x, y) ->
                f
                tolerance
                r'
                (showQ x ++ " " ++ showQ y)
                (g r x, g r y))
        tolerances
        distances
        pts
