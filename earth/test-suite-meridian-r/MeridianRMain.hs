import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Sphere.Meridian as S
import qualified Ellipsoid.Vincenty.Meridian as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Earth tests (with rationals)"
        [ testGroup "Haversines Math" [S.unitsR]
        , testGroup "Vincenty Math" [V.unitsR]
        ]
