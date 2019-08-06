import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Flat.Forbes as F
import qualified Sphere.Forbes as S
import qualified Ellipsoid.Forbes as E

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests"
        [ E.units
        , S.units
        , F.units
        ]
