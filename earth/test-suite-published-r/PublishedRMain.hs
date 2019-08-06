module PublishedRMain where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Flat.Published as F
import qualified Sphere.Published as S
import qualified Ellipsoid.Published as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests"
        [ E.unitsR
        , S.unitsR
        , F.unitsR
        ]
