import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Cylinder.Sphere.Sphere as S
import qualified Cylinder.Ellipsoid.Vincenty as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Cylinder tests (with doubles)"
        [ testGroup "Haversines Math" [S.outerUnits, S.innerUnits]
        , testGroup "Vincenty Math" [V.outerUnits, V.innerUnits]
        ]
