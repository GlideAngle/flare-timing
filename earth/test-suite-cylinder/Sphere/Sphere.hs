module Sphere.Sphere (properties, units, tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import Sphere.Published (publishedUnits)
import Sphere.Meridian (meridianUnits)
import Sphere.Touching (touchingUnits)
import Sphere.Coincident (coincidentUnits)
import Sphere.Disjoint (disjointUnits)
import Sphere.Greda (gredaUnits)
import Sphere.Forbes (forbesUnits)
import Sphere.Cylinder.Outer (outerCylinderUnits)
import Sphere.Cylinder.Inner (innerCylinderUnits)
import Props.Haversine (distancePoint, distanceHaversineF, distanceHaversine)

tests :: TestTree
tests =
    testGroup "On the FAI sphere using haversines"
    [ units
    , properties
    ]

properties :: TestTree
properties =
    testGroup "Property tests on the FAI sphere using haversines"
    [ qcProps
    ]

units :: TestTree
units =
    testGroup "Unit tests on the FAI sphere using haversines"
    [ publishedUnits
    , gredaUnits
    , forbesUnits
    , meridianUnits
    , disjointUnits
    , touchingUnits
    , coincidentUnits
    , outerCylinderUnits
    , innerCylinderUnits
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty
        "HaversineF distances, are not negative"
        distanceHaversineF

    , QC.testProperty
        "Haversine distances, are not negative"
        distanceHaversine

    , QC.testProperty
        "Zone distances, point-to-point, are not negative"
        distancePoint
    ]
