import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Cylinder.Sphere as S
import qualified Cylinder.Ellipsoid.Vincenty as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Cylinder tests (with rationals)"
        [ testGroup "Haversines Math"
            [ S.outerUnitsR
            , S.innerUnitsR
            ]
        , testGroup "Vincenty Math"
            [ V.outerUnitsR
            , V.innerUnitsR
            ]
        ]
