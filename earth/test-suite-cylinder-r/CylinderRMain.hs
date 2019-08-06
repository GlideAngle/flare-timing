module CylinderRMain where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Sphere.Sphere as S
import qualified Ellipsoid.Ellipsoid as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Cylinder tests with rational numbers"
        [ E.testsR
        , S.testsR
        ]
