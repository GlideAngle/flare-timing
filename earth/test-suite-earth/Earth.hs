module Earth (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Flat.Flat as F
import qualified Sphere.Sphere as S
import qualified Ellipsoid.Ellipsoid as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth Tests"
        [ properties
        , units
        ]

units :: TestTree
units =
    testGroup "Earth unit tests"
        [ E.units
        , S.units
        , F.units
        ]

properties :: TestTree
properties =
    testGroup "Earth property tests"
        [ E.properties
        , S.properties
        , F.properties
        ]
