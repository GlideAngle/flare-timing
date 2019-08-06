module Sphere.Disjoint (disjointUnits) where

import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure ((+:), (*:), u, negate', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), QRadius)
import Flight.Earth.Sphere (earthRadius)
import Zone (MkZone, QLL, showQ, dotZones, areaZones)
import Sphere.Touching (Overlay(..), separatedZones)
import Sphere.Span (sepD, sepR)

disjointUnits :: TestTree
disjointUnits =
    testGroup "Disjoint zones are separated"
    [ testGroup "With doubles"
        $ (uncurry fD <$> dotZones)
        ++ (uncurry gD <$> areaZones)
    , testGroup "With rationals"
        $ (uncurry fR <$> dotZones)
        ++ (uncurry gR <$> areaZones)
    ]
    where
        fD s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepD Disjoint delta radius)

        gD s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepD Disjoint delta radius)

        fR s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepR Disjoint delta radius)

        gR s =
            zonesDisjoint
                (s ++ " zones")
                (separatedZones sepR Disjoint delta radius)

type Disjoint a = String -> [[Zone a]] -> TestTree

delta :: (Real a, Fractional a) => Quantity a [u| 1 |]
delta = fromRational' . MkQuantity $ 1 % 100000000

radius :: (Real a, Fractional a) => Quantity a [u| 1 |]
radius = fromRational' . MkQuantity $ 2 % 1

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    [ ((z, z), (z, pos))
    , ((z, z), (z, neg))
    ]
    where
        z = [u| 0 rad |]

        pos = (radius +: delta) *: [u| 1 rad |]
        neg = negate' (radius +: delta) *: [u| 1 rad |]

distances :: (Real a, Fractional a) => [QRadius a [u| m |]]
distances =
    repeat earthRadius

zonesDisjoint
    :: (Enum a, Real a, Fractional a)
    => String
    -> Disjoint a
    -> MkZone a a
    -> TestTree
zonesDisjoint s f g =
    testGroup s
    $ zipWith
        (\r (x, y) -> f (showQ x ++ " " ++ showQ y) [[g r x, g r y]])
        distances
        pts
