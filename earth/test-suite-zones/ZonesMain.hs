import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Flat.Flat as F
import qualified Sphere.Sphere as S
import qualified Ellipsoid.Ellipsoid as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests"
        [ E.units
        , S.units
        , F.units
        ]
