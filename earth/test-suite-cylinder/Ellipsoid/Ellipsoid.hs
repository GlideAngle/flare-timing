module Ellipsoid.Ellipsoid (properties, units, tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Providers as QC

import Ellipsoid.Coincident (coincidentUnits)
import Ellipsoid.Meridian (meridianUnits)
import Ellipsoid.Published (publishedUnits)
import Ellipsoid.Forbes (forbesUnits)
import Ellipsoid.Cylinder.Outer (outerCylinderUnits)
import Ellipsoid.Cylinder.Inner (innerCylinderUnits)
import Props.Vincenty (distancePoint, distanceVincentyF, distanceVincenty)

tests :: TestTree
tests =
    testGroup
    "On the WGS84 ellipsoid using Vincenty's solution to the inverse geodetic problem"
    [ units
    , properties
    ]

properties :: TestTree
properties =
    testGroup "Property tests on the WGS84 ellipsoid using Vincenty's solution"
    [ qcProps
    ]

units :: TestTree
units =
    testGroup "Unit tests on the WGS84 ellipsoid using Vincenty's solution"
    [ publishedUnits
    , forbesUnits
    , coincidentUnits
    , meridianUnits
    , outerCylinderUnits
    , innerCylinderUnits
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "VincentyF distances, are not negative" distanceVincentyF
    , QC.singleTest
        "Vincenty distances, are not negative"
        $ QC.QC
        $ QC.withMaxSuccess 10
        $ QC.property distanceVincenty
    , QC.singleTest
        "Zone distances, point-to-point, are not negative"
        $ QC.QC
        $ QC.withMaxSuccess 10
        $ QC.property distancePoint
    ]
